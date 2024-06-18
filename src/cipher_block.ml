open Uncommon

module Block = struct

  module type Core = sig

    type ekey
    type dkey

    val of_secret   : string -> ekey * dkey
    val e_of_secret : string -> ekey
    val d_of_secret : string -> dkey

    val key   : int array
    val block : int

    (* XXX currently unsafe point *)
    val encrypt : key:ekey -> blocks:int -> string -> int -> bytes -> int -> unit
    val decrypt : key:dkey -> blocks:int -> string -> int -> bytes -> int -> unit
  end

  module type ECB = sig

    type key
    val of_secret : string -> key

    val key_sizes  : int array
    val block_size : int
    val encrypt : key:key -> string -> string
    val decrypt : key:key -> string -> string
    val encrypt_into : key:key -> string -> src_off:int -> bytes -> dst_off:int -> int -> unit
    val decrypt_into : key:key -> string -> src_off:int -> bytes -> dst_off:int -> int -> unit
    val unsafe_encrypt_into : key:key -> string -> src_off:int -> bytes -> dst_off:int -> int -> unit
    val unsafe_decrypt_into : key:key -> string -> src_off:int -> bytes -> dst_off:int -> int -> unit
  end

  module type CBC = sig

    type key
    val of_secret : string -> key

    val key_sizes  : int array
    val block_size : int

    val encrypt : key:key -> iv:string -> string -> string
    val decrypt : key:key -> iv:string -> string -> string
    val next_iv : ?off:int -> string -> iv:string -> string

    val encrypt_into : key:key -> iv:string -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
    val decrypt_into : key:key -> iv:string -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit

    val unsafe_encrypt_into : key:key -> iv:string -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
    val unsafe_decrypt_into : key:key -> iv:string -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
    val unsafe_encrypt_into_inplace : key:key -> iv:string ->
      bytes -> dst_off:int -> int -> unit
  end

  module type CTR = sig

    type key
    val of_secret : string -> key

    val key_sizes  : int array
    val block_size : int

    type ctr
    val add_ctr        : ctr -> int64 -> ctr
    val next_ctr       : ?off:int -> string -> ctr:ctr -> ctr
    val ctr_of_octets  : string -> ctr

    val stream  : key:key -> ctr:ctr -> int -> string
    val encrypt : key:key -> ctr:ctr -> string -> string
    val decrypt : key:key -> ctr:ctr -> string -> string

    val stream_into  : key:key -> ctr:ctr -> bytes -> off:int -> int -> unit
    val encrypt_into : key:key -> ctr:ctr -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
    val decrypt_into : key:key -> ctr:ctr -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit

    val unsafe_stream_into  : key:key -> ctr:ctr -> bytes -> off:int -> int -> unit
    val unsafe_encrypt_into : key:key -> ctr:ctr -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
    val unsafe_decrypt_into : key:key -> ctr:ctr -> string -> src_off:int ->
      bytes -> dst_off:int -> int -> unit
  end

  module type GCM = sig
    include Aead.AEAD

    val key_sizes  : int array
    val block_size : int
  end

  module type CCM16 = sig
    include Aead.AEAD

    val key_sizes  : int array
    val block_size : int
  end
end

module Counters = struct
  module type S = sig
    type ctr
    val size : int
    val add  : ctr -> int64 -> ctr
    val of_octets : string -> ctr
    val unsafe_count_into : ctr -> bytes -> off:int -> blocks:int -> unit
  end

  module C64be = struct
    type ctr = int64
    let size = 8
    let of_octets cs = String.get_int64_be cs 0
    let add = Int64.add
    let unsafe_count_into t buf ~off ~blocks =
      let ctr = Bytes.create 8 in
      Bytes.set_int64_be ctr 0 t;
      Native.count8be ~ctr buf ~off ~blocks
  end

  module C128be = struct
    type ctr = int64 * int64
    let size = 16
    let of_octets cs =
      let buf = Bytes.unsafe_of_string cs in
      Bytes.(get_int64_be buf 0, get_int64_be buf 8)
    let add (w1, w0) n =
      let w0'  = Int64.add w0 n in
      let flip = if Int64.logxor w0 w0' < 0L then w0' > w0 else w0' < w0 in
      ((if flip then Int64.succ w1 else w1), w0')
    let unsafe_count_into (w1, w0) buf ~off ~blocks =
      let ctr = Bytes.create 16 in
      Bytes.set_int64_be ctr 0 w1; Bytes.set_int64_be ctr 8 w0;
      Native.count16be ~ctr buf ~off ~blocks
  end

  module C128be32 = struct
    include C128be
    let add (w1, w0) n =
      let hi = 0xffffffff00000000L and lo = 0x00000000ffffffffL in
      (w1, Int64.(logor (logand hi w0) (add n w0 |> logand lo)))
    let unsafe_count_into (w1, w0) buf ~off ~blocks =
      let ctr = Bytes.create 16 in
      Bytes.set_int64_be ctr 0 w1; Bytes.set_int64_be ctr 8 w0;
      Native.count16be4 ~ctr buf ~off ~blocks
  end
end

let check_offset ~tag ~buf ~off ~len actual_len =
  if off < 0 then
    invalid_arg "%s: %s off %u < 0"
      tag buf off;
  if actual_len - off < len then
    invalid_arg "%s: %s length %u - off %u < len %u"
      tag buf actual_len off len
[@@inline]

module Modes = struct
  module ECB_of (Core : Block.Core) : Block.ECB = struct

    type key = Core.ekey * Core.dkey

    let (key_sizes, block_size) = Core.(key, block)

    let of_secret = Core.of_secret

    let unsafe_ecb xform key src src_off dst dst_off len =
      xform ~key ~blocks:(len / block_size) src src_off dst dst_off

    let ecb xform key src src_off dst dst_off len =
      if len mod block_size <> 0 then
        invalid_arg "ECB: length %u not of block size" len;
      check_offset ~tag:"ECB" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"ECB" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      unsafe_ecb xform key src src_off dst dst_off len

    let encrypt_into ~key:(key, _) src ~src_off dst ~dst_off len =
      ecb Core.encrypt key src src_off dst dst_off len

    let unsafe_encrypt_into ~key:(key, _) src ~src_off dst ~dst_off len =
      unsafe_ecb Core.encrypt key src src_off dst dst_off len

    let decrypt_into ~key:(_, key) src ~src_off dst ~dst_off len =
      ecb Core.decrypt key src src_off dst dst_off len

    let unsafe_decrypt_into ~key:(_, key) src ~src_off dst ~dst_off len =
      unsafe_ecb Core.decrypt key src src_off dst dst_off len

    let encrypt ~key src =
      let len = String.length src in
      let dst = Bytes.create len in
      encrypt_into ~key src ~src_off:0 dst ~dst_off:0 len;
      Bytes.unsafe_to_string dst

    let decrypt ~key src =
      let len = String.length src in
      let dst = Bytes.create len in
      decrypt_into ~key src ~src_off:0 dst ~dst_off:0 len;
      Bytes.unsafe_to_string dst
  end

  module CBC_of (Core : Block.Core) : Block.CBC = struct

    type key = Core.ekey * Core.dkey

    let (key_sizes, block_size) = Core.(key, block)
    let block = block_size

    let of_secret = Core.of_secret

    let check_block_size ~iv len =
      if String.length iv <> block then
        invalid_arg "CBC: IV length %u not of block size" (String.length iv);
      if len mod block <> 0 then
        invalid_arg "CBC: argument length %u not of block size"
          len
    [@@inline]

    let next_iv ?(off = 0) cs ~iv =
      check_block_size ~iv (String.length cs - off) ;
      if String.length cs > off then
        String.sub cs (String.length cs - block_size) block_size
      else iv

    let unsafe_encrypt_into_inplace ~key:(key, _) ~iv dst ~dst_off len =
      let rec loop iv iv_i dst_i = function
        | 0 -> ()
        | b ->
          Native.xor_into_bytes iv iv_i dst dst_i block ;
          Core.encrypt ~key ~blocks:1 (Bytes.unsafe_to_string dst) dst_i dst dst_i ;
          (loop [@tailcall]) (Bytes.unsafe_to_string dst) dst_i (dst_i + block) (b - 1)
      in
      loop iv 0 dst_off (len / block)

    let unsafe_encrypt_into ~key ~iv src ~src_off dst ~dst_off len =
      Bytes.unsafe_blit_string src src_off dst dst_off len;
      unsafe_encrypt_into_inplace ~key ~iv dst ~dst_off len

    let encrypt_into ~key ~iv src ~src_off dst ~dst_off len =
      check_block_size ~iv len;
      check_offset ~tag:"CBC" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"CBC" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      unsafe_encrypt_into ~key ~iv src ~src_off dst ~dst_off len

    let encrypt ~key ~iv src =
      let dst = Bytes.create (String.length src) in
      encrypt_into ~key ~iv src ~src_off:0 dst ~dst_off:0 (String.length src);
      Bytes.unsafe_to_string dst

    let unsafe_decrypt_into ~key:(_, key) ~iv src ~src_off dst ~dst_off len =
      let b = len / block in
      if b > 0 then begin
        Core.decrypt ~key ~blocks:b src src_off dst dst_off ;
        Native.xor_into_bytes iv 0 dst dst_off block ;
        Native.xor_into_bytes src src_off dst (dst_off + block) ((b - 1) * block) ;
      end

    let decrypt_into ~key ~iv src ~src_off dst ~dst_off len =
      check_block_size ~iv len;
      check_offset ~tag:"CBC" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"CBC" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      unsafe_decrypt_into ~key ~iv src ~src_off dst ~dst_off len

    let decrypt ~key ~iv src =
      let len = String.length src in
      let msg = Bytes.create len in
      decrypt_into ~key ~iv src ~src_off:0 msg ~dst_off:0 len;
      Bytes.unsafe_to_string msg
  end

  module CTR_of (Core : Block.Core) (Ctr : Counters.S) :
    Block.CTR with type key = Core.ekey and type ctr = Ctr.ctr =
  struct
    (* FIXME: CTR has more room for speedups. Like stitching. *)

    assert (Core.block = Ctr.size)
    type key = Core.ekey
    type ctr = Ctr.ctr

    let (key_sizes, block_size) = Core.(key, block)
    let of_secret = Core.e_of_secret

    let unsafe_stream_into ~key ~ctr buf ~off len =
      let blocks = imax 0 len / block_size in
      Ctr.unsafe_count_into ctr buf ~off ~blocks ;
      Core.encrypt ~key ~blocks (Bytes.unsafe_to_string buf) off buf off ;
      let slack = imax 0 len mod block_size in
      if slack <> 0 then begin
        let buf' = Bytes.create block_size in
        let ctr = Ctr.add ctr (Int64.of_int blocks) in
        Ctr.unsafe_count_into ctr buf' ~off:0 ~blocks:1 ;
        Core.encrypt ~key ~blocks:1 (Bytes.unsafe_to_string buf') 0 buf' 0 ;
        Bytes.unsafe_blit buf' 0 buf (off + blocks * block_size) slack
      end

    let stream_into ~key ~ctr buf ~off len =
      check_offset ~tag:"CTR" ~buf:"buf" ~off ~len (Bytes.length buf);
      unsafe_stream_into ~key ~ctr buf ~off len

    let stream ~key ~ctr n =
      let buf = Bytes.create n in
      unsafe_stream_into ~key ~ctr buf ~off:0 n;
      Bytes.unsafe_to_string buf

    let unsafe_encrypt_into ~key ~ctr src ~src_off dst ~dst_off len =
      unsafe_stream_into ~key ~ctr dst ~off:dst_off len;
      Uncommon.unsafe_xor_into src ~src_off dst ~dst_off len

    let encrypt_into ~key ~ctr src ~src_off dst ~dst_off len =
      check_offset ~tag:"CTR" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"CTR" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      unsafe_encrypt_into ~key ~ctr src ~src_off dst ~dst_off len

    let encrypt ~key ~ctr src =
      let len = String.length src in
      let dst = Bytes.create len in
      encrypt_into ~key ~ctr src ~src_off:0 dst ~dst_off:0 len;
      Bytes.unsafe_to_string dst

    let decrypt = encrypt

    let decrypt_into = encrypt_into

    let unsafe_decrypt_into = unsafe_encrypt_into

    let add_ctr = Ctr.add
    let next_ctr ?(off = 0) msg ~ctr =
      add_ctr ctr (Int64.of_int @@ (String.length msg - off) // block_size)
    let ctr_of_octets = Ctr.of_octets
  end

  module GHASH : sig
    type key
    val derive  : string -> key
    val digesti : key:key -> (string Uncommon.iter) -> string
    val digesti_off_len : key:key -> (string * int * int) Uncommon.iter -> string
    val tagsize : int
  end = struct
    type key = string
    let keysize = Native.GHASH.keysize ()
    let tagsize = 16
    let derive cs =
      assert (String.length cs >= tagsize);
      let k = Bytes.create keysize in
      Native.GHASH.keyinit cs k;
      Bytes.unsafe_to_string k
    let digesti_off_len ~key i =
      let res = Bytes.make tagsize '\x00' in
      i (fun (cs, off, len) -> Native.GHASH.ghash key res cs off len);
      Bytes.unsafe_to_string res
    let digesti ~key i =
      let res = Bytes.make tagsize '\x00' in
      i (fun cs -> Native.GHASH.ghash key res cs 0 (String.length cs));
      Bytes.unsafe_to_string res

  end

  module GCM_of (C : Block.Core) : Block.GCM = struct

    assert (C.block = 16)
    module CTR = CTR_of (C) (Counters.C128be32)

    type key = { key : C.ekey ; hkey : GHASH.key }

    let tag_size = GHASH.tagsize
    let key_sizes, block_size = C.(key, block)
    let z128 = String.make block_size '\x00'

    let of_secret cs =
      let h = Bytes.create block_size in
      let key = C.e_of_secret cs in
      C.encrypt ~key ~blocks:1 z128 0 h 0;
      { key ; hkey = GHASH.derive (Bytes.unsafe_to_string h) }

    let bits64 cs = Int64.of_int (String.length cs * 8)

    let pack64s a b =
      let cs = Bytes.create 16 in
      Bytes.set_int64_be cs 0 a;
      Bytes.set_int64_be cs 8 b;
      Bytes.unsafe_to_string cs

    let counter ~hkey nonce = match String.length nonce with
      | 0 -> invalid_arg "GCM: invalid nonce of length 0"
      | 12 ->
        let (w1, w2) = String.get_int64_be nonce 0, String.get_int32_be nonce 8 in
        (w1, Int64.(shift_left (of_int32 w2) 32 |> add 1L))
      | _  ->
        CTR.ctr_of_octets @@
        GHASH.digesti ~key:hkey @@ iter2 nonce (pack64s 0L (bits64 nonce))

    let unsafe_tag_into ~key ~hkey ~ctr ?(adata = "") cdata ~off ~len dst ~tag_off =
      CTR.unsafe_encrypt_into ~key ~ctr
        (GHASH.digesti_off_len ~key:hkey
           (iter3 (adata, 0, String.length adata) (cdata, off, len)
              (pack64s (bits64 adata) (Int64.of_int (len * 8)), 0, 16)))
        ~src_off:0 dst ~dst_off:tag_off tag_size

    let unsafe_authenticate_encrypt_into ~key:{ key; hkey } ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len =
      let ctr = counter ~hkey nonce in
      CTR.(unsafe_encrypt_into ~key ~ctr:(add_ctr ctr 1L) src ~src_off dst ~dst_off len);
      unsafe_tag_into ~key ~hkey ~ctr ?adata (Bytes.unsafe_to_string dst) ~off:dst_off ~len dst ~tag_off

    let authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len =
      check_offset ~tag:"GCM" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"GCM" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      check_offset ~tag:"GCM" ~buf:"dst tag" ~off:tag_off ~len:tag_size (Bytes.length dst);
      unsafe_authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len

    let authenticate_encrypt ~key ~nonce ?adata data =
      let l = String.length data in
      let dst = Bytes.create (l + tag_size) in
      unsafe_authenticate_encrypt_into ~key ~nonce ?adata data ~src_off:0 dst ~dst_off:0 ~tag_off:l l;
      Bytes.unsafe_to_string dst

    let authenticate_encrypt_tag ~key ~nonce ?adata data =
      let r = authenticate_encrypt ~key ~nonce ?adata data in
      String.sub r 0 (String.length data),
      String.sub r (String.length data) tag_size

    let unsafe_authenticate_decrypt_into ~key:{ key; hkey } ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len =
      let ctr = counter ~hkey nonce in
      CTR.(unsafe_encrypt_into ~key ~ctr:(add_ctr ctr 1L) src ~src_off dst ~dst_off len);
      let ctag = Bytes.create tag_size in
      unsafe_tag_into ~key ~hkey ~ctr ?adata src ~off:src_off ~len ctag ~tag_off:0;
      Eqaf.equal (String.sub src tag_off tag_size) (Bytes.unsafe_to_string ctag)

    let authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len =
      check_offset ~tag:"GCM" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"GCM" ~buf:"src tag" ~off:tag_off ~len:tag_size (String.length src);
      check_offset ~tag:"GCM" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      unsafe_authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len

    let authenticate_decrypt ~key ~nonce ?adata cdata =
      if String.length cdata < tag_size then
        None
      else
        let l = String.length cdata - tag_size in
        let data = Bytes.create l in
        if unsafe_authenticate_decrypt_into ~key ~nonce ?adata cdata ~src_off:0 ~tag_off:l data ~dst_off:0 l then
          Some (Bytes.unsafe_to_string data)
        else
          None

    let authenticate_decrypt_tag ~key ~nonce ?adata ~tag:tag_data cipher =
      let cdata = cipher ^ tag_data in
      authenticate_decrypt ~key ~nonce ?adata cdata
  end

  module CCM16_of (C : Block.Core) : Block.CCM16 = struct

    assert (C.block = 16)

    let tag_size = C.block

    type key = C.ekey

    let of_secret sec = C.e_of_secret sec

    let (key_sizes, block_size) = C.(key, block)

    let cipher ~key src ~src_off dst ~dst_off =
      C.encrypt ~key ~blocks:1 src src_off dst dst_off

    let unsafe_authenticate_encrypt_into ~key ~nonce ?(adata = "") src ~src_off dst ~dst_off ~tag_off len =
      Ccm.unsafe_generation_encryption_into ~cipher ~key ~nonce ~adata
        src ~src_off dst ~dst_off ~tag_off len

    let valid_nonce nonce =
      let nsize = String.length nonce in
      if nsize < 7 || nsize > 13 then
        invalid_arg "CCM: nonce length not between 7 and 13: %u" nsize

    let authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len =
      check_offset ~tag:"CCM" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"CCM" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      check_offset ~tag:"CCM" ~buf:"dst tag" ~off:tag_off ~len:tag_size (Bytes.length dst);
      valid_nonce nonce;
      unsafe_authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len

    let authenticate_encrypt ~key ~nonce ?adata cs =
      valid_nonce nonce;
      let l = String.length cs in
      let dst = Bytes.create (l + tag_size) in
      unsafe_authenticate_encrypt_into ~key ~nonce ?adata cs ~src_off:0 dst ~dst_off:0 ~tag_off:l l;
      Bytes.unsafe_to_string dst

    let authenticate_encrypt_tag ~key ~nonce ?adata cs =
      let res = authenticate_encrypt ~key ~nonce ?adata cs in
      String.sub res 0 (String.length cs), String.sub res (String.length cs) tag_size

    let unsafe_authenticate_decrypt_into ~key ~nonce ?(adata = "") src ~src_off ~tag_off dst ~dst_off len =
      Ccm.unsafe_decryption_verification_into ~cipher ~key ~nonce ~adata src ~src_off ~tag_off dst ~dst_off len

    let authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len =
      check_offset ~tag:"CCM" ~buf:"src" ~off:src_off ~len (String.length src);
      check_offset ~tag:"CCM" ~buf:"src tag" ~off:tag_off ~len:tag_size (String.length src);
      check_offset ~tag:"CCM" ~buf:"dst" ~off:dst_off ~len (Bytes.length dst);
      valid_nonce nonce;
      unsafe_authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len

    let authenticate_decrypt ~key ~nonce ?adata data =
      if String.length data < tag_size then
        None
      else
        let dlen = String.length data - tag_size in
        let dst = Bytes.create dlen in
        if authenticate_decrypt_into ~key ~nonce ?adata data ~src_off:0 ~tag_off:dlen dst ~dst_off:0 dlen then
          Some (Bytes.unsafe_to_string dst)
        else
          None

    let authenticate_decrypt_tag ~key ~nonce ?adata ~tag cs =
      authenticate_decrypt ~key ~nonce ?adata (cs ^ tag)
  end
end

module AES = struct

  module Core : Block.Core = struct

    let key   = [| 16; 24; 32 |]
    let block = 16

    type ekey = string * int
    type dkey = string * int

    let of_secret_with init key =
      let rounds =
        match String.length key with
        | 16 | 24 | 32 -> String.length key / 4 + 6
        | _ -> invalid_arg "AES.of_secret: key length %u" (String.length key)
      in
      let rk = Bytes.create (Native.AES.rk_s rounds) in
      init key rk rounds ;
      Bytes.unsafe_to_string rk, rounds

    let derive_d ?e buf rk rs = Native.AES.derive_d buf rk rs e

    let e_of_secret = of_secret_with Native.AES.derive_e
    let d_of_secret = of_secret_with (derive_d ?e:None)

    let of_secret secret =
      let (e, _) as ekey = e_of_secret secret in
      (ekey, of_secret_with (derive_d ~e) secret)

    (* XXX arg order ocaml<->c slows down *)
    (* XXX bounds checks *)

    let encrypt ~key:(e, rounds) ~blocks src off1 dst off2 =
      Native.AES.enc src off1 dst off2 e rounds blocks

    let decrypt ~key:(d, rounds) ~blocks src off1 dst off2 =
      Native.AES.dec src off1 dst off2 d rounds blocks

  end

  module ECB = Modes.ECB_of (Core)
  module CBC = Modes.CBC_of (Core)
  module CTR = Modes.CTR_of (Core) (Counters.C128be)
  module GCM = Modes.GCM_of (Core)
  module CCM16 = Modes.CCM16_of (Core)

end

module DES = struct

  module Core : Block.Core = struct

    let key   = [| 24 |]
    let block = 8

    type ekey = string
    type dkey = string

    let k_s = Native.DES.k_s ()

    let gen_of_secret ~direction key =
      if String.length key <> 24 then
        invalid_arg "DES.of_secret: key length %u" (String.length key) ;
      let key = Bytes.of_string key in
      let keybuf = Bytes.create k_s in
      Native.DES.des3key key direction keybuf;
      Bytes.unsafe_to_string keybuf

    let e_of_secret = gen_of_secret ~direction:0
    let d_of_secret = gen_of_secret ~direction:1

    let of_secret secret = (e_of_secret secret, d_of_secret secret)

    let encrypt ~key ~blocks src off1 dst off2 =
      Native.DES.ddes src off1 dst off2 blocks key

    let decrypt = encrypt
  end

  module ECB = Modes.ECB_of (Core)
  module CBC = Modes.CBC_of (Core)
  module CTR = Modes.CTR_of (Core) (Counters.C64be)

end

let accelerated =
  let flags =
    (match Native.misc_mode () with 1 -> [`XOR] | _ -> []) @
    (match Native.AES.mode () with 1 -> [`AES] | _ -> []) @
    (match Native.GHASH.mode () with 1 -> [`GHASH] | _ -> []) in
  flags
