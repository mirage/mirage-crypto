open OUnit2

open Mirage_crypto.Uncommon

open Test_common
open Test_common_pk

let n_encode_decode_selftest
    (type a) ~typ ~bound (rmod, nmod : a Fc.Rng.t * a Fc.Numeric.t) n =
  let module N = (val nmod) in
  let module R = (val rmod) in
  typ ^ "selftest" >:: times ~n @@ fun _ ->
    let r = R.gen bound in
    let s = N.(of_cstruct_be @@ to_cstruct_be r)
    and t = N.(of_cstruct_be @@ to_cstruct_be ~size:24 r) in
    assert_equal r s;
    assert_equal r t

let n_decode_reencode_selftest (type a) ~typ ~bytes (nmod : a Fc.Numeric.t) n =
  let module N = (val nmod) in
  typ ^ " selftest" >:: times ~n @@ fun _ ->
    let cs  = Mirage_crypto_rng.generate bytes in
    let cs' = N.(to_cstruct_be ~size:bytes @@ of_cstruct_be cs) in
    assert_cs_equal cs cs'

let random_n_selftest (type a) ~typ (m : a Fc.Rng.t) n (bounds : (a * a) list) =
  let module N = (val m) in
  typ ^ " selftest" >::: (
    bounds |> List.map @@ fun (lo, hi) ->
      "selftest" >:: times ~n @@ fun _ ->
        let x = N.gen_r lo hi in
        if x < lo || x >= hi then assert_failure "range error"
  )

let int_safe_bytes = Sys.word_size // 8 - 1

let suite = [
  "Numeric extraction 1" >::: [
    n_encode_decode_selftest
      ~typ:"int"   ~bound:max_int (Fc.Rng.int, Fc.Numeric.int) 2000 ;
    n_encode_decode_selftest
      ~typ:"int32" ~bound:Int32.max_int (Fc.Rng.int32, Fc.Numeric.int32) 2000 ;
    n_encode_decode_selftest
      ~typ:"int64" ~bound:Int64.max_int (Fc.Rng.int64, Fc.Numeric.int64) 2000 ;
    n_encode_decode_selftest
      ~typ:"z"     ~bound:Z.(of_int64 Int64.max_int) (Fc.Rng.z, Fc.Numeric.z) 2000 ;
  ] ;

  "Numeric extraction 2" >::: [
    n_decode_reencode_selftest ~typ:"int"   ~bytes:int_safe_bytes Fc.Numeric.int 2000 ;
    n_decode_reencode_selftest ~typ:"int32" ~bytes:4  Fc.Numeric.int32 2000 ;
    n_decode_reencode_selftest ~typ:"int64" ~bytes:8  Fc.Numeric.int64 2000 ;
    n_decode_reencode_selftest ~typ:"z"     ~bytes:37 Fc.Numeric.z     2000 ;
  ];

  "RNG extraction" >::: [
    random_n_selftest ~typ:"int" Fc.Rng.int 1000 [
      (1, 2); (0, 129); (7, 136); (0, 536870913);
    ] ;
    random_n_selftest ~typ:"int32" Fc.Rng.int32 1000 [
      (7l, 136l); (0l, 536870913l);
    ] ;
    random_n_selftest ~typ:"int64" Fc.Rng.int64 1000 [
      (7L, 136L); (0L, 536870913L); (0L, 2305843009213693953L);
    ] ;
    random_n_selftest ~typ:"Z" Fc.Rng.z 1000 [
      Z.(of_int 7, of_int 135);
      Z.(of_int 0, of_int 536870913);
      Z.(of_int 0, of_int64 2305843009213693953L)
    ] ;
  ]
]
