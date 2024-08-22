open OUnit2

(* open Mirage_crypto.Uncommon *)
open Mirage_crypto_pk

open Test_common

let vz = Z.of_string_base 16

module Null = struct

  type g = string ref

  let block = 1

  let create ?time:_ () = ref ""

  let generate_into ~g buf ~off n =
    try
      Bytes.blit_string !g 0 buf off n;
      g := String.sub !g n (String.length !g - n)
    with Invalid_argument _ -> raise Mirage_crypto_rng.Unseeded_generator

  let reseed ~g buf = g := !g ^ buf

  let seeded ~g = String.length !g > 0

  let accumulate ~g _source = `Acc (reseed ~g)

  let pools = 0
end

let random_is seed =
  Mirage_crypto_rng.create ~seed:seed (module Null)

let gen_paillier ~bits =
  let key = Paillier.(generate ~bits ()) in
  assert_equal
    ~msg:Printf.(sprintf "key size not %d bits" bits)
    bits Paillier.(priv_bits (snd key)) ;
  key

let paillier_selftest ~bits n =
  "selftest" >:: times ~n @@ fun _ ->
    let msg = Z.(~$100) in
    let key = gen_paillier ~bits in
    let enc = Paillier.(encrypt ~pub_key:(fst key) ~msg ()) in
    let dec = Paillier.(decrypt ~priv_key:(snd key) ~c:enc) in

    assert_equal
      ~msg:Printf.(sprintf "failed decryption with")
      msg dec


let suite = [
  "Paillier" >::: [
    paillier_selftest ~bits:89   100  ;
    paillier_selftest ~bits:131  100  ;
    paillier_selftest ~bits:1024 10   ;
    paillier_selftest ~bits:2048 10   ;
  ] ;
]
