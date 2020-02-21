open Mirage_crypto.Uncommon

type bits = int

module type N = sig
  type t
  val gen      : ?g:Mirage_crypto_rng.g -> t -> t
  val gen_r    : ?g:Mirage_crypto_rng.g -> t -> t -> t
  val gen_bits : ?g:Mirage_crypto_rng.g -> ?msb:int -> int -> t
end

module Make_N (N : Numeric.S) = struct

  type t = N.t

  let gen ?g n =
    if n < N.one then invalid_arg "Rng.gen: non-positive: %a" N.pp_print n;

    let bs     = Mirage_crypto_rng.block g in
    let bits   = N.(bits (pred n)) in
    let octets = bits // 8 in
    let batch  =
      if Mirage_crypto_rng.strict g then octets else 2 * octets // bs * bs
    in

    let rec attempt cs =
      if cs.Cstruct.len >= octets then
        let x = N.of_cstruct_be ~bits cs in
        if x < n then x else attempt (Cstruct.shift cs octets)
      else attempt (Mirage_crypto_rng.generate ?g batch) in
    attempt (Mirage_crypto_rng.generate ?g batch)

  let rec gen_r ?g a b =
    if Mirage_crypto_rng.strict g then
      let x = gen ?g b in if x < a then gen_r ?g a b else x
    else N.(a + gen ?g (b - a))

  let gen_bits ?g ?(msb = 0) bits =
    let res = Mirage_crypto_rng.generate ?g (bits // 8) in
    Cs.set_msb msb res ;
    N.of_cstruct_be ~bits res

end

module Int   = Make_N (Numeric.Int  )
module Int32 = Make_N (Numeric.Int32)
module Int64 = Make_N (Numeric.Int64)
module ZN    = Make_N (Numeric.Z    )

(* Invalid combinations of ~bits and ~msb will loop forever, but there is no
 * way to quickly determine upfront whether there are any primes in the
 * interval.
 * XXX Probability is distributed as inter-prime gaps. So?
*)
let rec prime ?g ?(msb = 1) bits =
  let p = Z.(nextprime @@ ZN.gen_bits ?g ~msb bits) in
  if p < Z.(one lsl bits) then p else prime ?g ~msb bits

(* XXX Add ~msb param for p? *)
let rec safe_prime ?g bits =
  let q = prime ?g ~msb:1 (bits - 1) in
  let p = Z.(q * ~$2 + ~$1) in
  if Numeric.pseudoprime p then (q, p) else safe_prime ?g bits

(*     |+ Pocklington primality test specialized for `a = 2`. +|
       if Z.(gcd (of_int 3) p = one) then (q, p)
       else safe_prime ?g ~bits *)

module Z = ZN
