(* Based on https://github.com/abeaumont/ocaml-chacha.git *)

open Uncommon

let block = 64

type key = string

let of_secret a =
  let len = String.length a in
  if len <> 16 && len <> 32 then
    invalid_arg "Chacha20.of_secret: key length %u (must be either 32 bytes \
                 with 12 bytes nonce and 32 bit counter (IETF mode), or 16 \
                 or 32 bytes key with a 8 byte nonce and 64bit counter)" len;
  a

let chacha20_block state idx key_stream =
  Native.Chacha.round 10 state key_stream idx

let init ctr ~key ~nonce ~blocks =
  let ctr_off = 48 in
  let set_ctr32 b v = Bytes.set_int32_le b ctr_off v
  and set_ctr64 b v = Bytes.set_int64_le b ctr_off v
  in
  let inc32 b = set_ctr32 b (Int32.add (Bytes.get_int32_le b ctr_off) 1l)
  and inc64 b = set_ctr64 b (Int64.add (Bytes.get_int64_le b ctr_off) 1L)
  in
  let check_blocks max =
    if Int64.unsigned_compare (Int64.of_int blocks) max > 0 then
      invalid_arg "Chacha20: too many blocks"
  in
  let s, key, init_ctr, nonce_off, inc =
    match String.length key, String.length nonce, Int64.shift_right ctr 32 = 0L with
    | 32, 12, true ->
      check_blocks 0x100000000L;
      let ctr = Int64.to_int32 ctr in
      "expand 32-byte k", key, (fun b -> set_ctr32 b ctr), 52, inc32
    | 32, 12, false ->
      invalid_arg "Counter too big for IETF mode (32 bit counter)"
    | 32, 8, _ ->
      check_blocks Int64.minus_one;
      "expand 32-byte k", key, (fun b -> set_ctr64 b ctr), 56, inc64
    | 16, 8, _ ->
      let k = key ^ key in
      check_blocks Int64.minus_one;
      "expand 16-byte k", k, (fun b -> set_ctr64 b ctr), 56, inc64
    | _ -> invalid_arg "Valid parameters are nonce 12 bytes and key 32 bytes \
                        (counter 32 bit), or nonce 8 byte and key 16 or 32 \
                        bytes (counter 64 bit)."
  in
  let state = Bytes.create block in
  Bytes.unsafe_blit_string s 0 state 0 16 ;
  Bytes.unsafe_blit_string key 0 state 16 32 ;
  init_ctr state ;
  Bytes.unsafe_blit_string nonce 0 state nonce_off (String.length nonce) ;
  state, inc

let crypt_into ~key ~nonce ~ctr src ~src_off dst ~dst_off len =
  let block_count = len // block in
  let state, inc = init ctr ~key ~nonce ~blocks:block_count in
  let last_len =
    let last = len mod block in
    if last = 0 then block else last
  in
  let rec loop i = function
    | 0 -> ()
    | 1 ->
      if last_len = block then begin
        chacha20_block state (dst_off + i) dst ;
        Native.xor_into_bytes src (src_off + i) dst (dst_off + i) block
      end else begin
        let buf = Bytes.create block in
        chacha20_block state 0 buf ;
        Native.xor_into_bytes src (src_off + i) buf 0 last_len ;
        Bytes.unsafe_blit buf 0 dst (dst_off + i) last_len
      end
    | n ->
      chacha20_block state (dst_off + i) dst ;
      Native.xor_into_bytes src (src_off + i) dst (dst_off + i) block ;
      inc state;
      (loop [@tailcall]) (i + block) (n - 1)
  in
  loop 0 block_count

let crypt ~key ~nonce ?(ctr = 0L) data =
  let l = String.length data in
  let res = Bytes.create l in
  crypt_into ~key ~nonce ~ctr data ~src_off:0 res ~dst_off:0 l;
  Bytes.unsafe_to_string res

module P = Poly1305.It

let tag_size = P.mac_size

let generate_poly1305_key ~key ~nonce =
  crypt ~key ~nonce (String.make 32 '\000')

let mac_into ~key ~adata src ~src_off len dst ~dst_off =
  let pad16 l =
    let len = l mod 16 in
    if len = 0 then "" else String.make (16 - len) '\000'
  and len_buf =
    let data = Bytes.create 16 in
    Bytes.set_int64_le data 0 (Int64.of_int (String.length adata));
    Bytes.set_int64_le data 8 (Int64.of_int len);
    Bytes.unsafe_to_string data
  in
  let p1 = pad16 (String.length adata) and p2 = pad16 len in
  P.unsafe_mac_into ~key [ adata, 0, String.length adata ;
                           p1, 0, String.length p1 ;
                           src, src_off, len ;
                           p2, 0, String.length p2 ;
                           len_buf, 0, String.length len_buf ]
    dst ~dst_off

let check_aead_blocks_and_key key nonce len =
  let blocks = len // block in
  match String.length key, String.length nonce with
  | 32, 12 ->
    if Int64.of_int blocks > 0xffffffffL then invalid_arg "Chacha20: too many blocks"
  | 16, 8 | 32, 8 ->
    if Int64.unsigned_compare (Int64.of_int blocks) Int64.minus_one > 0 then
      invalid_arg "Chacha20: too many blocks"
  | n, k ->
    invalid_arg "ChaCha20: invalid key and nonce length. Supported are: key 32 \
                 bytes and nonce 8 bytes OR key 16 or 32 bytes and nonce 12 \
                 bytes. Provided: key %u bytes and nonce %u bytes" k n
[@@inline always]

let unsafe_authenticate_encrypt_into ~key ~nonce ?(adata = "") src ~src_off dst ~dst_off ~tag_off len =
  check_aead_blocks_and_key key nonce len;
  let poly1305_key = generate_poly1305_key ~key ~nonce in
  crypt_into ~key ~nonce ~ctr:1L src ~src_off dst ~dst_off len;
  mac_into ~key:poly1305_key ~adata (Bytes.unsafe_to_string dst) ~src_off:dst_off len dst ~dst_off:tag_off

let authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len =
  if src_off < 0 || dst_off < 0 || tag_off < 0 || len < 0 then
    invalid_arg "Chacha20: negative offset or length";
  if String.length src - src_off < len then
    invalid_arg "Chacha20: src length %u - src_off %u < len %u"
      (String.length src) src_off len;
  if Bytes.length dst - dst_off < len then
    invalid_arg "Chacha20: dst length %u - dst_off %u < len %u"
      (Bytes.length dst) dst_off len;
  if Bytes.length dst - tag_off < tag_size then
    invalid_arg "Chacha20: dst length %u - tag_off %u < tag_size %u"
      (Bytes.length dst) tag_off tag_size;
  unsafe_authenticate_encrypt_into ~key ~nonce ?adata src ~src_off dst ~dst_off ~tag_off len

let authenticate_encrypt ~key ~nonce ?adata data =
  let l = String.length data in
  let dst = Bytes.create (l + tag_size) in
  unsafe_authenticate_encrypt_into ~key ~nonce ?adata data ~src_off:0 dst ~dst_off:0 ~tag_off:l l;
  Bytes.unsafe_to_string dst

let authenticate_encrypt_tag ~key ~nonce ?adata data =
  let r = authenticate_encrypt ~key ~nonce ?adata data in
  String.sub r 0 (String.length data), String.sub r (String.length data) tag_size

let unsafe_authenticate_decrypt_into ~key ~nonce ?(adata = "") src ~src_off ~tag_off dst ~dst_off len =
  check_aead_blocks_and_key key nonce len;
  let poly1305_key = generate_poly1305_key ~key ~nonce in
  let ctag = Bytes.create tag_size in
  mac_into ~key:poly1305_key ~adata src ~src_off len ctag ~dst_off:0;
  let r = Eqaf.equal (String.sub src tag_off tag_size) (Bytes.unsafe_to_string ctag) in
  if r then crypt_into ~key ~nonce ~ctr:1L src ~src_off dst ~dst_off len;
  r

let authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len =
  if src_off < 0 || dst_off < 0 || tag_off < 0 || len < 0 then
    invalid_arg "Chacha20: negative offset or length";
  if String.length src - src_off < len then
    invalid_arg "Chacha20: src length %u - src_off %u < len %u"
      (String.length src) src_off len;
  if Bytes.length dst - dst_off < len then
    invalid_arg "Chacha20: dst length %u - dst_off %u < len %u"
      (Bytes.length dst) dst_off len;
  if String.length src - tag_off < tag_size then
    invalid_arg "Chacha20: src length %u - tag_off %u < tag_size %u"
      (String.length src) tag_off tag_size;
  unsafe_authenticate_decrypt_into ~key ~nonce ?adata src ~src_off ~tag_off dst ~dst_off len

let authenticate_decrypt ~key ~nonce ?adata data =
  if String.length data < tag_size then
    None
  else
    let l = String.length data - tag_size in
    let r = Bytes.create l in
    if unsafe_authenticate_decrypt_into ~key ~nonce ?adata data ~src_off:0 ~tag_off:l r ~dst_off:0 l then
      Some (Bytes.unsafe_to_string r)
    else
      None

let authenticate_decrypt_tag ~key ~nonce ?adata ~tag data =
  let cdata = data ^ tag in
  authenticate_decrypt ~key ~nonce ?adata cdata
