open OUnit2

let () =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    Mirage_crypto.accelerated

let suite =
  "All" >::: [
    "Basic" >::: Test_base.suite;
    "Cipher" >::: Test_cipher.suite;
  ]

let () =
  run_test_tt_main suite
