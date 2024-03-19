open Uncommon

let block_size = 16

let flags bit6 len1 len2 =
  bit6 lsl 6 + len1 lsl 3 + len2

let encode_len buf ~off size value =
  let rec ass num = function
    | 0 -> Bytes.set_uint8 buf off num
    | m ->
      Bytes.set_uint8 buf (off + m) (num land 0xff);
      ass (num lsr 8) (pred m)
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

let pad_block ?(off = 0) b =
  let size = Bytes.length b - off in
  Bytes.concat Bytes.empty [ b ; Bytes.make (size // block_size * block_size) '\x00' ]

let pad_block_str ~off b =
  let size = String.length b - off in
  String.concat "" [ b ; String.make (size // block_size * block_size) '\x00' ]

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
    Bytes.blit_string a 0 buf (off + llen) (String.length a);
    Bytes.fill buf (off + llen + String.length a) to_pad '\000'

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

let crypto_core ~cipher ~mode ~key ~nonce ~maclen ~adata data =
  let datalen = String.length data in
  let cbcheader = prepare_header nonce adata datalen maclen in
  let dst = Bytes.create datalen in

  let small_q = 15 - String.length nonce in
  let ctr_flag_val = flags 0 0 (small_q - 1) in
  let ctrblock i block =
    Bytes.set_uint8 block 0 ctr_flag_val;
    Bytes.unsafe_blit_string nonce 0 block 1 (String.length nonce);
    encode_len block ~off:(String.length nonce + 1) small_q i;
    cipher ~key (Bytes.unsafe_to_string block) ~src_off:0 block ~dst_off:0
  in

  let cbc iv src_off block dst_off =
    xor_into iv ~src_off block ~dst_off block_size ;
    cipher ~key (Bytes.unsafe_to_string block) ~src_off:dst_off block ~dst_off
  in

  let cbcprep =
    let rec doit iv iv_off block block_off =
      match Bytes.length block - block_off with
      | 0 -> Bytes.sub iv iv_off block_size
      | _ ->
         cbc (Bytes.unsafe_to_string iv) iv_off block block_off;
         doit block block_off block (block_off + block_size)
    in
    doit (Bytes.make block_size '\x00') 0 cbcheader 0
  in

  let rec loop iv ctr src src_off dst dst_off=
    let cbcblock, cbc_off =
      match mode with
      | Encrypt -> src, src_off
      | Decrypt -> Bytes.unsafe_to_string dst, dst_off
    in
    match String.length src - src_off with
    | 0 -> iv
    | x when x < block_size ->
       (* TODO: the pads below are scary - we're only interested in the last bytes (from dst_off .. end) *)
       let ctrbl = pad_block ~off:dst_off dst in
       ctrblock ctr ctrbl ;
       Bytes.unsafe_blit ctrbl 0 dst dst_off x ;
       xor_into src ~src_off dst ~dst_off x ;
       let cbblock = pad_block_str ~off:cbc_off cbcblock in
       cbc cbblock cbc_off iv 0 ;
       iv
    | _ ->
       ctrblock ctr dst ;
       xor_into src ~src_off dst ~dst_off block_size ;
       cbc cbcblock cbc_off iv 0 ;
       loop iv
            (succ ctr)
            src (src_off + block_size)
            dst (dst_off + block_size)
  in
  let last = loop cbcprep 1 data 0 dst 0 in
  let t = Bytes.sub last 0 maclen in
  (dst, t)

let crypto_t t nonce cipher key =
  let ctr = gen_ctr nonce 0 in
  cipher ~key (Bytes.unsafe_to_string ctr) ~src_off:0 ctr ~dst_off:0 ;
  xor_into (Bytes.unsafe_to_string ctr) t (Bytes.length t)

let valid_nonce nonce =
  let nsize = String.length nonce in
  if nsize < 7 || nsize > 13 then
    invalid_arg "CCM: nonce length not between 7 and 13: %u" nsize

let generation_encryption ~cipher ~key ~nonce ~maclen ~adata data =
  valid_nonce nonce;
  let cdata, t = crypto_core ~cipher ~mode:Encrypt ~key ~nonce ~maclen ~adata data in
  crypto_t t nonce cipher key ;
  Bytes.unsafe_to_string cdata, Bytes.unsafe_to_string t

let decryption_verification ~cipher ~key ~nonce ~maclen ~adata ~tag data =
  valid_nonce nonce;
  let cdata, t = crypto_core ~cipher ~mode:Decrypt ~key ~nonce ~maclen ~adata data in
  crypto_t t nonce cipher key ;
  match Eqaf.equal tag (Bytes.unsafe_to_string t) with
  | true  -> Some (Bytes.unsafe_to_string cdata)
  | false -> None
