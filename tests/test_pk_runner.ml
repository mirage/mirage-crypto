open OUnit2

let suite =
  "All" >::: [
    "Numeric" >::: Test_numeric.suite;
    "DHE" >::: Test_dh.suite;
    "DSA" >::: Test_dsa.suite;
    "RSA" >::: Test_rsa.suite;
    "Paillier" >::: Test_paillier.suite;
  ]

let () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  run_test_tt_main suite
