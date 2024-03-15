(* Based on https://github.com/abeaumont/ocaml-chacha.git *)

open Uncommon

let block = 64

type key = string

let of_secret a = a

let chacha20_block state idx key_stream =
  Native.Chacha.round 10 state key_stream idx

let init ctr ~key ~nonce =
  let ctr_off = 48 in
  let set_ctr32 b v = Bytes.set_int32_le b ctr_off v
  and set_ctr64 b v = Bytes.set_int64_le b ctr_off v
  in
  let inc32 b = set_ctr32 b (Int32.add (Bytes.get_int32_le b ctr_off) 1l)
  and inc64 b = set_ctr64 b (Int64.add (Bytes.get_int64_le b ctr_off) 1L)
  in
  let s, key, init_ctr, nonce_off, inc =
    match String.length key, String.length nonce, Int64.shift_right ctr 32 = 0L with
    | 32, 12, true ->
      let ctr = Int64.to_int32 ctr in
      "expand 32-byte k", key, (fun b -> set_ctr32 b ctr), 52, inc32
    | 32, 12, false ->
      invalid_arg "Counter too big for IETF mode (32 bit counter)"
    | 32, 8, _ ->
      "expand 32-byte k", key, (fun b -> set_ctr64 b ctr), 56, inc64
    | 16, 8, _ ->
      let k = key ^ key in
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

let crypt ~key ~nonce ?(ctr = 0L) data =
  let state, inc = init ctr ~key ~nonce in
  let l = String.length data in
  let block_count = l // block in
  let last_len =
    let last = l mod block in
    if last = 0 then block else last
  in
  let res = Bytes.create l in
  let rec loop i = function
    | 0 -> ()
    | 1 ->
      if last_len = block then begin
        chacha20_block state i res ;
        Native.xor_into_bytes data i res i block
      end else begin
        let buf = Bytes.create block in
        chacha20_block state 0 buf ;
        Native.xor_into_bytes data i buf 0 last_len ;
        Bytes.unsafe_blit buf 0 res i last_len
      end
    | n ->
      chacha20_block state i res ;
      Native.xor_into_bytes data i res i block ;
      inc state;
      loop (i + block) (n - 1)
  in
  loop 0 block_count ;
  Bytes.unsafe_to_string res

module P = Poly1305.It

let generate_poly1305_key ~key ~nonce =
  crypt ~key ~nonce (String.make 32 '\000')

let mac ~key ~adata ciphertext =
  let pad16 b =
    let len = String.length b mod 16 in
    if len = 0 then "" else String.make (16 - len) '\000'
  and len =
    let data = Bytes.create 16 in
    Bytes.set_int64_le data 0 (Int64.of_int (String.length adata));
    Bytes.set_int64_le data 8 (Int64.of_int (String.length ciphertext));
    Bytes.unsafe_to_string data
  in
  P.macl ~key [ adata ; pad16 adata ; ciphertext ; pad16 ciphertext ; len ]

let authenticate_encrypt_tag ~key ~nonce ?(adata = "") data =
  let poly1305_key = generate_poly1305_key ~key ~nonce in
  let ciphertext = crypt ~key ~nonce ~ctr:1L data in
  let mac = mac ~key:poly1305_key ~adata ciphertext in
  ciphertext, mac

let authenticate_encrypt ~key ~nonce ?adata data =
  let cdata, ctag = authenticate_encrypt_tag ~key ~nonce ?adata data in
  cdata ^ ctag

let authenticate_decrypt_tag ~key ~nonce ?(adata = "") ~tag data =
  let poly1305_key = generate_poly1305_key ~key ~nonce in
  let ctag = mac ~key:poly1305_key ~adata data in
  let plain = crypt ~key ~nonce ~ctr:1L data in
  if Eqaf.equal tag ctag then Some plain else None

let authenticate_decrypt ~key ~nonce ?adata data =
  if String.length data < P.mac_size then
    None
  else
    let cipher, tag =
      let p = String.length data - P.mac_size in
      String.sub data 0 p, String.sub data p P.mac_size
    in
    authenticate_decrypt_tag ~key ~nonce ?adata ~tag cipher

let tag_size = P.mac_size
