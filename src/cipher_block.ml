open Uncommon

module S = struct

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
  end

  module type CBC = sig

    type key
    val of_secret : string -> key

    val key_sizes  : int array
    val block_size : int

    val encrypt : key:key -> iv:string -> string -> string
    val decrypt : key:key -> iv:string -> string -> string
    val next_iv : iv:string -> string -> string
  end

  module type CTR = sig

    type key
    val of_secret : string -> key

    type ctr

    val key_sizes  : int array
    val block_size : int

    val stream  : key:key -> ctr:ctr -> int -> string
    val encrypt : key:key -> ctr:ctr -> string -> string
    val decrypt : key:key -> ctr:ctr -> string -> string

    val add_ctr        : ctr -> int64 -> ctr
    val next_ctr       : ctr:ctr -> string -> ctr
    val ctr_of_octets  : string -> ctr
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
    val unsafe_count_into : ctr -> bytes -> int -> blocks:int -> unit
  end

  let _tmp = Bytes.make 16 '\x00'

  module C64be = struct
    type ctr = int64
    let size = 8
    (* Until OCaml 4.13 is lower bound*)
    let of_octets cs = Bytes.get_int64_be (Bytes.unsafe_of_string cs) 0
    let add = Int64.add
    let unsafe_count_into t buf off ~blocks =
      Bytes.set_int64_be _tmp 0 t;
      Native.count8be _tmp buf off ~blocks
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
    let unsafe_count_into (w1, w0) buf off ~blocks =
      Bytes.set_int64_be _tmp 0 w1; Bytes.set_int64_be _tmp 8 w0;
      Native.count16be _tmp buf off ~blocks
  end

  module C128be32 = struct
    include C128be
    let add (w1, w0) n =
      let hi = 0xffffffff00000000L and lo = 0x00000000ffffffffL in
      (w1, Int64.(logor (logand hi w0) (add n w0 |> logand lo)))
    let unsafe_count_into (w1, w0) buf off ~blocks =
      Bytes.set_int64_be _tmp 0 w1; Bytes.set_int64_be _tmp 8 w0;
      Native.count16be4 _tmp buf off ~blocks
  end
end

module Modes = struct
  module ECB_of (Core : S.Core) : S.ECB = struct

    type key = Core.ekey * Core.dkey

    let (key_sizes, block_size) = Core.(key, block)

    let of_secret = Core.of_secret

    let (encrypt, decrypt) =
      let ecb xform key src =
        let n = String.length src in
        if n mod block_size <> 0 then invalid_arg "ECB: length %u" n;
        let dst = Bytes.create n in
        xform ~key ~blocks:(n / block_size) src 0 dst 0 ;
        Bytes.unsafe_to_string dst
      in
      (fun ~key:(key, _) src -> ecb Core.encrypt key src),
      (fun ~key:(_, key) src -> ecb Core.decrypt key src)

  end

  module CBC_of (Core : S.Core) : S.CBC = struct

    type key = Core.ekey * Core.dkey

    let (key_sizes, block_size) = Core.(key, block)
    let block = block_size

    let of_secret = Core.of_secret

    let bounds_check ~iv cs =
      if String.length iv <> block then invalid_arg "CBC: IV length %u" (String.length iv);
      if String.length cs mod block <> 0 then
        invalid_arg "CBC: argument length %u" (String.length cs)

    let next_iv ~iv cs =
      bounds_check ~iv cs ;
      if String.length cs > 0 then
        String.sub cs (String.length cs - block_size) block_size
      else iv

    let encrypt ~key:(key, _) ~iv src =
      bounds_check ~iv src ;
      let dst = Bytes.of_string src in
      let rec loop iv iv_i dst_i = function
        0 -> ()
      | b -> Native.xor_into_bytes iv iv_i dst dst_i block ;
             Core.encrypt ~key ~blocks:1 (Bytes.unsafe_to_string dst) dst_i dst dst_i ;
             loop (Bytes.unsafe_to_string dst) dst_i (dst_i + block) (b - 1)
      in
      loop iv 0 0 (Bytes.length dst / block) ;
      Bytes.unsafe_to_string dst

    let decrypt ~key:(_, key) ~iv src =
      bounds_check ~iv src ;
      let msg = Bytes.create (String.length src)
      and b   = String.length src / block in
      if b > 0 then begin
        Core.decrypt ~key ~blocks:b src 0 msg 0 ;
        Native.xor_into_bytes iv 0 msg 0 block ;
        Native.xor_into_bytes src 0 msg block ((b - 1) * block) ;
      end ;
      Bytes.unsafe_to_string msg

  end

  module CTR_of (Core : S.Core) (Ctr : Counters.S) :
    S.CTR with type key = Core.ekey and type ctr = Ctr.ctr =
  struct
    (* FIXME: CTR has more room for speedups. Like stitching. *)

    assert (Core.block = Ctr.size)
    type key = Core.ekey
    type ctr = Ctr.ctr

    let (key_sizes, block_size) = Core.(key, block)
    let of_secret = Core.e_of_secret

    let stream ~key ~ctr n =
      let blocks = imax 0 n / block_size in
      let buf = Bytes.create n in
      Ctr.unsafe_count_into ctr ~blocks buf 0 ;
      Core.encrypt ~key ~blocks (Bytes.unsafe_to_string buf) 0 buf 0 ;
      let slack = imax 0 n mod block_size in
      if slack <> 0 then begin
        let buf' = Bytes.create block_size in
        let ctr = Ctr.add ctr (Int64.of_int blocks) in
        Ctr.unsafe_count_into ctr ~blocks:1 buf' 0 ;
        Core.encrypt ~key ~blocks:1 (Bytes.unsafe_to_string buf') 0 buf' 0 ;
        Bytes.blit buf' 0 buf (blocks * block_size) slack
      end;
      Bytes.unsafe_to_string buf

    let encrypt ~key ~ctr src =
      let res = Bytes.unsafe_of_string (stream ~key ~ctr (String.length src)) in
      Native.xor_into_bytes src 0 res 0 (String.length src) ;
      Bytes.unsafe_to_string res

    let decrypt = encrypt

    let add_ctr = Ctr.add
    let next_ctr ~ctr msg = add_ctr ctr (Int64.of_int @@ String.length msg // block_size)
    let ctr_of_octets = Ctr.of_octets
  end

  module GHASH : sig
    type key
    val derive  : string -> key
    val digesti : key:key -> (string Uncommon.iter) -> string
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
    let hash0 = Bytes.make tagsize '\x00'
    let digesti ~key i =
      let res = Bytes.copy hash0 in
      i (fun cs -> Native.GHASH.ghash key res cs (String.length cs));
      Bytes.unsafe_to_string res
  end

  module GCM_of (C : S.Core) : S.GCM = struct

    let _ = assert (C.block = 16)
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

    (* OCaml 4.13 *)
    let string_get_int64 s idx =
      Bytes.get_int64_be (Bytes.unsafe_of_string s) idx
    let string_get_int32 s idx =
      Bytes.get_int32_be (Bytes.unsafe_of_string s) idx

    let counter ~hkey nonce = match String.length nonce with
      | 0 -> invalid_arg "GCM: invalid nonce of length 0"
      | 12 ->
        let (w1, w2) = string_get_int64 nonce 0, string_get_int32 nonce 8 in
        (w1, Int64.(shift_left (of_int32 w2) 32 |> add 1L))
      | _  ->
        CTR.ctr_of_octets @@
        GHASH.digesti ~key:hkey @@ iter2 nonce (pack64s 0L (bits64 nonce))

    let tag ~key ~hkey ~ctr ?(adata = "") cdata =
      CTR.encrypt ~key ~ctr @@
        GHASH.digesti ~key:hkey @@
          iter3 adata cdata (pack64s (bits64 adata) (bits64 cdata))

    let authenticate_encrypt_tag ~key:{ key; hkey } ~nonce ?adata data =
      let ctr   = counter ~hkey nonce in
      let cdata = CTR.(encrypt ~key ~ctr:(add_ctr ctr 1L) data) in
      let ctag  = tag ~key ~hkey ~ctr ?adata cdata in
      cdata, ctag

    let authenticate_encrypt ~key ~nonce ?adata data =
      let cdata, ctag = authenticate_encrypt_tag ~key ~nonce ?adata data in
      cdata ^ ctag

    let authenticate_decrypt_tag ~key:{ key; hkey } ~nonce ?adata ~tag:tag_data cipher =
      let ctr  = counter ~hkey nonce in
      let data = CTR.(encrypt ~key ~ctr:(add_ctr ctr 1L) cipher) in
      let ctag = tag ~key ~hkey ~ctr ?adata cipher in
      if Eqaf.equal tag_data ctag then Some data else None

    let authenticate_decrypt ~key ~nonce ?adata cdata =
      if String.length cdata < tag_size then
        None
      else
        let cipher, tag =
          String.sub cdata 0 (String.length cdata - tag_size),
          String.sub cdata (String.length cdata - tag_size) tag_size
        in
        authenticate_decrypt_tag ~key ~nonce ?adata ~tag cipher
  end

  module CCM16_of (C : S.Core) : S.CCM16 = struct

    let _ = assert (C.block = 16)

    let tag_size = 16

    type key = C.ekey

    let of_secret sec = C.e_of_secret sec

    let (key_sizes, block_size) = C.(key, block)

    let cipher ~key src ~src_off dst ~dst_off =
      if String.length src - src_off < block_size || Bytes.length dst - dst_off < block_size then
        invalid_arg "src len %u, dst len %u" (String.length src - src_off) (Bytes.length dst - dst_off);
      C.encrypt ~key ~blocks:1 src src_off dst dst_off

    let authenticate_encrypt_tag ~key ~nonce ?(adata = "") cs =
      Ccm.generation_encryption ~cipher ~key ~nonce ~maclen:tag_size ~adata cs

    let authenticate_encrypt ~key ~nonce ?adata cs =
      let cdata, ctag = authenticate_encrypt_tag ~key ~nonce ?adata cs in
      cdata ^ ctag

    let authenticate_decrypt_tag ~key ~nonce ?(adata = "") ~tag cs =
      Ccm.decryption_verification ~cipher ~key ~nonce ~maclen:tag_size ~adata ~tag cs

    let authenticate_decrypt ~key ~nonce ?adata data =
      if String.length data < tag_size then
        None
      else
        let data, tag =
          String.sub data 0 (String.length data - tag_size),
          String.sub data (String.length data - tag_size) tag_size
        in
        authenticate_decrypt_tag ~key ~nonce ?adata ~tag data
  end
end

module AES = struct

  module Core : S.Core = struct

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

  module Core : S.Core = struct

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
      Native.DES.des3key key direction ;
      Native.DES.cp3key keybuf ;
      Bytes.unsafe_to_string keybuf

    let e_of_secret = gen_of_secret ~direction:0
    let d_of_secret = gen_of_secret ~direction:1

    let of_secret secret = (e_of_secret secret, d_of_secret secret)

    let encrypt ~key ~blocks src off1 dst off2 =
      Native.DES.use3key key ;
      Native.DES.ddes src off1 dst off2 blocks

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
