open Uncommon

let block_size = 16

let flags bit6 len1 len2 =
  bit6 lsl 6 + len1 lsl 3 + len2

let encode_len buf ~off size value =
  let rec ass num = function
    | 0 -> Bytes.set_uint8 buf off num
    | m ->
      Bytes.set_uint8 buf (off + m) (num land 0xff);
      (ass [@tailcall]) (num lsr 8) (pred m)
  in
  ass value (pred size)

let set_format buf ?(off = 0) nonce flag_val value =
  let n = String.length nonce in
  let small_q = 15 - n in
  (* first octet block:
     0          : flags
     1..15 - q  : N
     16 - q..15 : Q *)
  Bytes.set_uint8 buf off flag_val;
  Bytes.unsafe_blit_string nonce 0 buf (off + 1) n;
  encode_len buf ~off:(off + n + 1) small_q value

let gen_adata a =
  let llen, set_llen =
    match String.length a with
    | x when x < (1 lsl 16 - 1 lsl 8) ->
      2, (fun buf off -> Bytes.set_uint16_be buf off x)
    | x when Sys.int_size < 32 || x < (1 lsl 32) ->
      6, (fun buf off ->
          Bytes.set_uint16_be buf off 0xfffe;
          Bytes.set_int32_be buf (off + 2) (Int32.of_int x))
    | x ->
      10, (fun buf off ->
          Bytes.set_uint16_be buf off 0xffff;
          Bytes.set_int64_be buf (off + 2) (Int64.of_int x))
  in
  let to_pad =
    let leftover = (llen + String.length a) mod block_size in
    block_size - leftover
  in
  llen + String.length a + to_pad,
  fun buf off ->
    set_llen buf off;
    Bytes.unsafe_blit_string a 0 buf (off + llen) (String.length a);
    Bytes.unsafe_fill buf (off + llen + String.length a) to_pad '\000'

let gen_ctr nonce i =
  let n = String.length nonce in
  let small_q = 15 - n in
  let flag_val = flags 0 0 (small_q - 1) in
  let buf = Bytes.create 16 in
  set_format buf nonce flag_val i;
  buf

let prepare_header nonce adata plen tlen =
  let small_q = 15 - String.length nonce in
  let b6 = if String.length adata = 0 then 0 else 1 in
  let flag_val = flags b6 ((tlen - 2) / 2) (small_q - 1) in
  if String.length adata = 0 then
    let hdr = Bytes.create 16 in
    set_format hdr nonce flag_val plen;
    hdr
  else
    let len, set = gen_adata adata in
    let buf = Bytes.create (16 + len) in
    set_format buf nonce flag_val plen;
    set buf 16;
    buf

type mode = Encrypt | Decrypt

let crypto_core_into ~cipher ~mode ~key ~nonce ~adata src ~src_off dst ~dst_off len =
  let cbcheader = prepare_header nonce adata len block_size in

  let small_q = 15 - String.length nonce in
  let ctr_flag_val = flags 0 0 (small_q - 1) in
  let ctrblock i block dst_off =
    Bytes.set_uint8 block dst_off ctr_flag_val;
    Bytes.unsafe_blit_string nonce 0 block (dst_off + 1) (String.length nonce);
    encode_len block ~off:(dst_off + String.length nonce + 1) small_q i;
    cipher ~key (Bytes.unsafe_to_string block) ~src_off:dst_off block ~dst_off
  in

  let cbc iv src_off block dst_off =
    unsafe_xor_into iv ~src_off block ~dst_off block_size ;
    cipher ~key (Bytes.unsafe_to_string block) ~src_off:dst_off block ~dst_off
  in

  let iv =
    let rec doit iv iv_off block block_off =
      match Bytes.length block - block_off with
      | 0 -> Bytes.sub iv iv_off block_size
      | _ ->
         cbc (Bytes.unsafe_to_string iv) iv_off block block_off;
         (doit [@tailcall]) block block_off block (block_off + block_size)
    in
    doit (Bytes.make block_size '\x00') 0 cbcheader 0
  in

  let rec loop ctr src src_off dst dst_off len =
    let cbcblock, cbc_off =
      match mode with
      | Encrypt -> src, src_off
      | Decrypt -> Bytes.unsafe_to_string dst, dst_off
    in
    if len = 0 then
      ()
    else if len < block_size then begin
      let buf = Bytes.make block_size '\x00' in
      Bytes.unsafe_blit dst dst_off buf 0 len ;
      ctrblock ctr buf 0 ;
      Bytes.unsafe_blit buf 0 dst dst_off len ;
      unsafe_xor_into src ~src_off dst ~dst_off len ;
      Bytes.unsafe_blit_string cbcblock cbc_off buf 0 len ;
      Bytes.unsafe_fill buf len (block_size - len) '\x00';
      cbc (Bytes.unsafe_to_string buf) 0 iv 0
    end else begin
      ctrblock ctr dst dst_off ;
      unsafe_xor_into src ~src_off dst ~dst_off block_size ;
      cbc cbcblock cbc_off iv 0 ;
      (loop [@tailcall]) (succ ctr) src (src_off + block_size) dst (dst_off + block_size) (len - block_size)
    end
  in
  loop 1 src src_off dst dst_off len;
  iv

let crypto_core ~cipher ~mode ~key ~nonce ~adata data =
  let datalen = String.length data in
  let dst = Bytes.create datalen in
  let t = crypto_core_into ~cipher ~mode ~key ~nonce ~adata data ~src_off:0 dst ~dst_off:0 datalen in
  dst, t

let crypto_t t nonce cipher key =
  let ctr = gen_ctr nonce 0 in
  cipher ~key (Bytes.unsafe_to_string ctr) ~src_off:0 ctr ~dst_off:0 ;
  unsafe_xor_into (Bytes.unsafe_to_string ctr) ~src_off:0 t ~dst_off:0 (Bytes.length t)

let unsafe_generation_encryption_into ~cipher ~key ~nonce ~adata src ~src_off dst ~dst_off ~tag_off len =
  let t = crypto_core_into ~cipher ~mode:Encrypt ~key ~nonce ~adata src ~src_off dst ~dst_off len in
  crypto_t t nonce cipher key ;
  Bytes.unsafe_blit t 0 dst tag_off block_size

let unsafe_decryption_verification_into ~cipher ~key ~nonce ~adata src ~src_off ~tag_off dst ~dst_off len =
  let tag = String.sub src tag_off block_size in
  let t = crypto_core_into ~cipher ~mode:Decrypt ~key ~nonce ~adata src ~src_off dst ~dst_off len in
  crypto_t t nonce cipher key ;
  Eqaf.equal tag (Bytes.unsafe_to_string t)
