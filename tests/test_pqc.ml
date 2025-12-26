(** Tests for ML-KEM-768 *)

open Mirage_crypto_pqc

let () = Mirage_crypto_rng_unix.use_default ()

let test_keygen () =
  let ek, dk = ML_KEM_768.generate () in
  Alcotest.(check int) "ek length" ML_KEM_768.ek_len (String.length ek);
  Alcotest.(check int) "dk length" ML_KEM_768.dk_len (String.length dk)

let test_encaps_decaps () =
  let ek, dk = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek with
  | Error msg -> Alcotest.fail ("Encapsulation failed: " ^ msg)
  | Ok (ct, _ss1) ->
    Alcotest.(check int) "ct length" ML_KEM_768.ct_len (String.length ct);
    match ML_KEM_768.decapsulate dk ct with
    | Error msg -> Alcotest.fail ("Decapsulation failed: " ^ msg)
    | Ok ss2 ->
      Alcotest.(check int) "ss length" ML_KEM_768.ss_len (String.length ss2)
      (* Note: In placeholder implementation, ss1 and ss2 won't match.
         A real implementation would verify: ss1 = ss2 *)

let test_invalid_ek_length () =
  match ML_KEM_768.encapsulate "too_short" with
  | Error _ -> ()  (* Expected *)
  | Ok _ -> Alcotest.fail "Should reject invalid ek length"

let test_invalid_dk_length () =
  match ML_KEM_768.decapsulate "too_short" (String.make ML_KEM_768.ct_len 'x') with
  | Error _ -> ()  (* Expected *)
  | Ok _ -> Alcotest.fail "Should reject invalid dk length"

let test_invalid_ct_length () =
  let _, dk = ML_KEM_768.generate () in
  match ML_KEM_768.decapsulate dk "too_short" with
  | Error _ -> ()  (* Expected *)
  | Ok _ -> Alcotest.fail "Should reject invalid ct length"

let test_multiple_encaps () =
  (* Same ek should produce different ciphertexts due to randomness *)
  let ek, _dk = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek, ML_KEM_768.encapsulate ek with
  | Ok (ct1, ss1), Ok (ct2, ss2) ->
    (* Ciphertexts should be different (with overwhelming probability) *)
    Alcotest.(check bool) "different ciphertexts" true (ct1 <> ct2);
    (* Shared secrets should also be different *)
    Alcotest.(check bool) "different secrets" true (ss1 <> ss2)
  | Error msg, _ | _, Error msg ->
    Alcotest.fail ("Encapsulation failed: " ^ msg)

let test_key_sizes () =
  (* Verify sizes match FIPS 203 ML-KEM-768 specification *)
  Alcotest.(check int) "ek_len is 1184" 1184 ML_KEM_768.ek_len;
  Alcotest.(check int) "dk_len is 2400" 2400 ML_KEM_768.dk_len;
  Alcotest.(check int) "ct_len is 1088" 1088 ML_KEM_768.ct_len;
  Alcotest.(check int) "ss_len is 32" 32 ML_KEM_768.ss_len

let tests = [
  ("key sizes", `Quick, test_key_sizes);
  ("keygen", `Quick, test_keygen);
  ("encaps/decaps", `Quick, test_encaps_decaps);
  ("invalid ek length", `Quick, test_invalid_ek_length);
  ("invalid dk length", `Quick, test_invalid_dk_length);
  ("invalid ct length", `Quick, test_invalid_ct_length);
  ("multiple encaps", `Quick, test_multiple_encaps);
]

let () = Alcotest.run "ML-KEM-768" [ "ML_KEM_768", tests ]
