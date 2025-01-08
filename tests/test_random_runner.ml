open OUnit2

open Mirage_crypto

open Test_common

let sample arr =
  let ix =
    Randomconv.int ~bound:(Array.length arr) Mirage_crypto_rng.generate
  in
  arr.(ix)

(* randomized selfies *)

let ecb_selftest (m : (module Block.ECB)) n =
  let module C = ( val m ) in
  "selftest" >:: times ~n @@ fun _ ->
    let data  = Mirage_crypto_rng.generate (C.block_size * 8)
    and key   = C.of_secret @@ Mirage_crypto_rng.generate (sample C.key_sizes) in
    let data' =
      C.( data |> encrypt ~key |> encrypt ~key
               |> decrypt ~key |> decrypt ~key ) in
    assert_oct_equal ~msg:"ecb mismatch" data data'

let cbc_selftest (m : (module Block.CBC)) n  =
  let module C = ( val m ) in
  "selftest" >:: times ~n @@ fun _ ->
    let data = Mirage_crypto_rng.generate (C.block_size * 8)
    and iv   = Mirage_crypto_rng.generate C.block_size
    and key  = C.of_secret @@ Mirage_crypto_rng.generate (sample C.key_sizes) in
    assert_oct_equal ~msg:"CBC e->e->d->d" data
      C.( data |> encrypt ~key ~iv |> encrypt ~key ~iv
               |> decrypt ~key ~iv |> decrypt ~key ~iv );
    let (d1, d2) =
      String.sub data 0 (C.block_size * 4),
      String.sub data (C.block_size * 4) (String.length data - C.block_size * 4)
    in
    assert_oct_equal ~msg:"CBC chain"
      C.(encrypt ~key ~iv data)
      C.( let e1 = encrypt ~key ~iv d1 in
          e1 ^ encrypt ~key ~iv:(next_iv ~iv e1) d2)

let ctr_selftest (m : (module Block.CTR)) n =
  let module M = (val m) in
  let bs = M.block_size in
  "selftest" >:: times ~n @@ fun _ ->
    let key  = M.of_secret @@ Mirage_crypto_rng.generate (sample M.key_sizes)
    and ctr  = Mirage_crypto_rng.generate bs |> M.ctr_of_octets
    and data = Mirage_crypto_rng.(generate @@ bs + Randomconv.int ~bound:(20 * bs) Mirage_crypto_rng.generate) in
    let enc = M.encrypt ~key ~ctr data in
    let dec = M.decrypt ~key ~ctr enc in
    assert_oct_equal ~msg:"CTR e->d" data dec;
    let (d1, d2) =
      let s = bs * Randomconv.int ~bound:(String.length data / bs) Mirage_crypto_rng.generate in
      String.sub data 0 s, String.sub data s (String.length data - s)
    in
    assert_oct_equal ~msg:"CTR chain" enc @@
      M.encrypt ~key ~ctr d1 ^ M.encrypt ~key ~ctr:(M.next_ctr ~ctr d1) d2

let ctr_offsets (type c) ~zero (m : (module Block.CTR with type ctr = c)) n =
  let module M = (val m) in
  "offsets" >:: fun _ ->
    let key = M.of_secret @@ Mirage_crypto_rng.generate M.key_sizes.(0) in
    for i = 0 to n - 1 do
      let ctr = match i with
        | 0 -> M.add_ctr zero (-1L)
        | _ -> Mirage_crypto_rng.generate M.block_size |> M.ctr_of_octets
      and gap = Randomconv.int ~bound:64 Mirage_crypto_rng.generate in
      let s1 = M.stream ~key ~ctr ((gap + 1) * M.block_size)
      and s2 = M.stream ~key ~ctr:(M.add_ctr ctr (Int64.of_int gap)) M.block_size in
      assert_oct_equal ~msg:"shifted stream"
        String.(sub s1 (gap * M.block_size) M.block_size) s2
    done

let xor_selftest n =
  "selftest" >:: times ~n @@ fun _ ->

    let n         = Randomconv.int ~bound:30 Mirage_crypto_rng.generate in
    let (x, y, z) = Mirage_crypto_rng.(generate n, generate n, generate n) in

    let xyz  = Uncommon.(xor (xor x y) z)
    and xyz' = Uncommon.(xor x (xor y z)) in
    let x1   = Uncommon.(xor xyz (xor y z))
    and x2   = Uncommon.(xor (xor z y) xyz) in

    assert_oct_equal ~msg:"assoc" xyz xyz' ;
    assert_oct_equal ~msg:"invert" x x1 ;
    assert_oct_equal ~msg:"commut" x1 x2

let suite =
  "All" >::: [
    "XOR" >::: [ xor_selftest 300 ] ;
    "3DES-ECB" >::: [ ecb_selftest (module DES.ECB) 100 ] ;

    "3DES-CBC" >::: [ cbc_selftest (module DES.CBC) 100 ] ;

    "3DES-CTR" >::: [ ctr_selftest (module DES.CTR) 100;
                      ctr_offsets  (module DES.CTR) 100 ~zero:0L; ] ;

    "AES-ECB" >::: [ ecb_selftest (module AES.ECB) 100 ] ;
    "AES-CBC" >::: [ cbc_selftest (module AES.CBC) 100 ] ;
    "AES-CTR" >::: [ ctr_selftest (module AES.CTR) 100;
                     ctr_offsets  (module AES.CTR) 100 ~zero:(0L, 0L) ] ;

  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  run_test_tt_main suite
