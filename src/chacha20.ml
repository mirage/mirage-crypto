(* Based on https://github.com/abeaumont/ocaml-chacha.git *)

open Uncommon

let block = 64

let chacha20_block state idx key_stream =
  Native.Chacha.round 10 state.Cstruct.buffer 0 key_stream.Cstruct.buffer idx

let init ctr ~key ~nonce =
  let ctr_off = 48 in
  let set_ctr32 b v = Cstruct.LE.set_uint32 b ctr_off v
  and set_ctr64 b v = Cstruct.LE.set_uint64 b ctr_off v
  in
  let inc32 b = set_ctr32 b (Int32.add (Cstruct.LE.get_uint32 b ctr_off) 1l)
  and inc64 b = set_ctr64 b (Int64.add (Cstruct.LE.get_uint64 b ctr_off) 1L)
  in
  let s, key, init_ctr, nonce_off, inc =
    match Cstruct.len key, Cstruct.len nonce, Int64.shift_right ctr 32 = 0L with
    | 32, 12, true ->
      let ctr = Int64.to_int32 ctr in
      "expand 32-byte k", key, (fun b -> set_ctr32 b ctr), 52, inc32
    | 32, 12, false ->
      invalid_arg "Counter too big for IETF mode (32 bit counter)"
    | 32, 8, _ ->
      "expand 32-byte k", key, (fun b -> set_ctr64 b ctr), 56, inc64
    | 16, 8, _ ->
      let k = Cstruct.append key key in
      "expand 16-byte k", k, (fun b -> set_ctr64 b ctr), 56, inc64
    | _ -> invalid_arg "Valid parameters are nonce 12 bytes and key 32 bytes \
                        (counter 32 bit), or nonce 8 byte and key 16 or 32 \
                        bytes (counter 64 bit)."
  in
  let state = Cstruct.create block in
  Cstruct.blit_from_string s 0 state 0 16 ;
  Cstruct.blit key 0 state 16 32 ;
  init_ctr state ;
  Cstruct.blit nonce 0 state nonce_off (Cstruct.len nonce) ;
  state, inc

let crypt ~key ~nonce ?(ctr = 0L) data =
  let state, inc = init ctr ~key ~nonce in
  let l = Cstruct.len data in
  let block_count = l // block in
  let len = block * block_count in
  let key_stream = Cstruct.create_unsafe len in
  let rec loop i = function
    | 0 -> ()
    | n ->
      chacha20_block state i key_stream ;
      Native.xor_into data.buffer (data.off + i) key_stream.buffer i block ;
      inc state;
      loop (i + block) (n - 1)
  in
  loop 0 block_count ;
  Cstruct.sub key_stream 0 l
