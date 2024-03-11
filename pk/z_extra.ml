open Mirage_crypto.Uncommon

let bit_bound z = Z.size z * 64

(* revise once OCaml 4.13 is the lower bound *)
let string_get_int64_be buf idx =
  Bytes.get_int64_be (Bytes.unsafe_of_string buf) idx

let string_get_int32_be buf idx =
  Bytes.get_int32_be (Bytes.unsafe_of_string buf) idx

let string_get_uint16_be buf idx =
  Bytes.get_uint16_be (Bytes.unsafe_of_string buf) idx

let string_get_uint8 buf idx =
  Bytes.get_uint8 (Bytes.unsafe_of_string buf) idx

let of_octets_be ?bits buf =
  let rec loop acc i = function
    | b when b >= 64 ->
      let x = string_get_int64_be buf i in
      let x = Z.of_int64_unsigned Int64.(shift_right_logical x 8) in
      loop Z.(x + acc lsl 56) (i + 7) (b - 56)
    | b when b >= 32 ->
      let x = string_get_int32_be buf i in
      let x = Z.of_int32_unsigned Int32.(shift_right_logical x 8) in
      loop Z.(x + acc lsl 24) (i + 3) (b - 24)
    | b when b >= 16 ->
      let x = Z.of_int (string_get_uint16_be buf i) in
      loop Z.(x + acc lsl 16) (i + 2) (b - 16)
    | b when b >= 8  ->
      let x = Z.of_int (string_get_uint8 buf i) in
      loop Z.(x + acc lsl 8 ) (i + 1) (b - 8 )
    | b when b > 0   ->
      let x = string_get_uint8 buf i and b' = 8 - b in
      Z.(of_int x asr b' + acc lsl b)
    | _              -> acc in
  loop Z.zero 0 @@ match bits with
  | None   -> String.length buf * 8
  | Some b -> imin b (String.length buf * 8)

let byte1 = Z.of_int64 0xffL
and byte2 = Z.of_int64 0xffffL
and byte3 = Z.of_int64 0xffffffL
and byte7 = Z.of_int64 0xffffffffffffffL

let into_octets_be n buf =
  let rec write n = function
    | i when i >= 7 ->
      Bytes.set_int64_be buf (i - 7) Z.(to_int64_unsigned (n land byte7)) ;
      write Z.(n asr 56) (i - 7)
    | i when i >= 3 ->
      Bytes.set_int32_be buf (i - 3) Z.(to_int32_unsigned (n land byte3)) ;
      write Z.(n asr 24) (i - 3)
    | i when i >= 1 ->
      Bytes.set_uint16_be buf (i - 1) Z.(to_int (n land byte2)) ;
      write Z.(n asr 16) (i - 2)
    | 0 -> Bytes.set_uint8 buf 0 Z.(to_int (n land byte1)) ;
    | _ -> ()
  in
  write n (Bytes.length buf - 1)

let to_octets_be ?size n =
  let buf = Bytes.create @@ match size with
    | Some s -> imax 0 s
    | None   -> Z.numbits n // 8 in
  into_octets_be n buf;
  Bytes.unsafe_to_string buf

(* Handbook of Applied Cryptography, Table 4.4:
 * Miller-Rabin rounds for composite probability <= 1/2^80. *)
let pseudoprime z =
  let i = match Z.numbits z with
    | i when i >= 1300 ->  2
    | i when i >=  850 ->  3
    | i when i >=  650 ->  4
    | i when i >=  350 ->  8
    | i when i >=  250 -> 12
    | i when i >=  150 -> 18
    | _                -> 27 in
  Z.probab_prime z i <> 0

(* strip_factor ~f x = (s, t), where x = f^s t *)
let strip_factor ~f x =
  let rec go n x =
    let (x1, r) = Z.div_rem x f in
    if r = Z.zero then go (succ n) x1 else Ok (n, x)
  in
  if Z.(~$2) <= f then
    go 0 x
  else
    Error (`Msg ("factor_count: f: " ^ Z.to_string f))

let gen ?g n =
  if n < Z.one then invalid_arg "Rng.gen: non-positive: %a" Z.pp_print n;
  let bs     = Mirage_crypto_rng.block g in
  let bits   = Z.(numbits (pred n)) in
    let octets = bits // 8 in
    let batch  =
      if Mirage_crypto_rng.strict g then octets else 2 * octets // bs * bs
    in
    let rec attempt buf =
      if String.length buf >= octets then
        let x = of_octets_be ~bits buf in
        if x < n then x else attempt (String.sub buf octets (String.length buf - octets))
      else attempt (Mirage_crypto_rng.generate ?g batch) in
    attempt (Mirage_crypto_rng.generate ?g batch)

let rec gen_r ?g a b =
  if Mirage_crypto_rng.strict g then
    let x = gen ?g b in if x < a then gen_r ?g a b else x
  else Z.(a + gen ?g (b - a))


let set_msb bits buf =
  if bits > 0 then
    let n = Bytes.length buf in
    let rec go width = function
      | i when i = n     -> ()
      | i when width < 8 ->
        Bytes.set_uint8 buf i (Bytes.get_uint8 buf i lor (0xff lsl (8 - width)))
      | i ->
        Bytes.set_uint8 buf i 0xff ;
        go (width - 8) (succ i)
    in
    go bits 0

let gen_bits ?g ?(msb = 0) bits =
  let bytelen = bits // 8 in
  let buf = Bytes.create bytelen in
  Mirage_crypto_rng.generate_into ?g buf ~off:0 bytelen;
  set_msb msb buf ;
  of_octets_be ~bits (Bytes.unsafe_to_string buf)

(* Invalid combinations of ~bits and ~msb will loop forever, but there is no
 * way to quickly determine upfront whether there are any primes in the
 * interval.
 * XXX Probability is distributed as inter-prime gaps. So?
*)
let rec prime ?g ?(msb = 1) bits =
  let p = Z.(nextprime @@ gen_bits ?g ~msb bits) in
  if p < Z.(one lsl bits) then p else prime ?g ~msb bits

(* XXX Add ~msb param for p? *)
let rec safe_prime ?g bits =
  let q = prime ?g ~msb:1 (bits - 1) in
  let p = Z.(q * ~$2 + ~$1) in
  if pseudoprime p then (q, p) else safe_prime ?g bits
