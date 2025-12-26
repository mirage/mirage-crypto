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
  | Ok (ct, ss1) ->
    Alcotest.(check int) "ct length" ML_KEM_768.ct_len (String.length ct);
    Alcotest.(check int) "ss1 length" ML_KEM_768.ss_len (String.length ss1);
    match ML_KEM_768.decapsulate dk ct with
    | Error msg -> Alcotest.fail ("Decapsulation failed: " ^ msg)
    | Ok ss2 ->
      Alcotest.(check int) "ss2 length" ML_KEM_768.ss_len (String.length ss2);
      (* Verify shared secrets match *)
      Alcotest.(check string) "shared secrets match" ss1 ss2

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
  let ek, dk = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek, ML_KEM_768.encapsulate ek with
  | Ok (ct1, ss1), Ok (ct2, ss2) ->
    (* Ciphertexts should be different (with overwhelming probability) *)
    Alcotest.(check bool) "different ciphertexts" true (ct1 <> ct2);
    (* Shared secrets should also be different *)
    Alcotest.(check bool) "different secrets" true (ss1 <> ss2);
    (* But both should decrypt correctly *)
    (match ML_KEM_768.decapsulate dk ct1 with
     | Ok ss1' -> Alcotest.(check string) "ss1 decaps correctly" ss1 ss1'
     | Error msg -> Alcotest.fail ("Decapsulation of ct1 failed: " ^ msg));
    (match ML_KEM_768.decapsulate dk ct2 with
     | Ok ss2' -> Alcotest.(check string) "ss2 decaps correctly" ss2 ss2'
     | Error msg -> Alcotest.fail ("Decapsulation of ct2 failed: " ^ msg))
  | Error msg, _ | _, Error msg ->
    Alcotest.fail ("Encapsulation failed: " ^ msg)

let test_key_sizes () =
  (* Verify sizes match FIPS 203 ML-KEM-768 specification *)
  Alcotest.(check int) "ek_len is 1184" 1184 ML_KEM_768.ek_len;
  Alcotest.(check int) "dk_len is 2400" 2400 ML_KEM_768.dk_len;
  Alcotest.(check int) "ct_len is 1088" 1088 ML_KEM_768.ct_len;
  Alcotest.(check int) "ss_len is 32" 32 ML_KEM_768.ss_len

let test_implicit_rejection () =
  (* Test that modified ciphertext produces different (but valid) shared secret *)
  let ek, dk = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek with
  | Error msg -> Alcotest.fail ("Encapsulation failed: " ^ msg)
  | Ok (ct, ss_good) ->
    (* Modify the ciphertext slightly *)
    let ct_bad = Bytes.of_string ct in
    let byte0 = Char.code (Bytes.get ct_bad 0) in
    Bytes.set ct_bad 0 (Char.chr ((byte0 lxor 1) land 0xFF));
    let ct_bad = Bytes.to_string ct_bad in
    match ML_KEM_768.decapsulate dk ct_bad with
    | Error msg -> Alcotest.fail ("Decapsulation should not error: " ^ msg)
    | Ok ss_bad ->
      (* Should produce a valid 32-byte output *)
      Alcotest.(check int) "ss_bad length" 32 (String.length ss_bad);
      (* But it should be different from the correct shared secret *)
      Alcotest.(check bool) "implicit rejection works" true (ss_good <> ss_bad)

let test_multiple_keypairs () =
  (* Different keypairs should work independently *)
  let ek1, dk1 = ML_KEM_768.generate () in
  let ek2, dk2 = ML_KEM_768.generate () in
  (* Keys should be different *)
  Alcotest.(check bool) "different ek" true (ek1 <> ek2);
  Alcotest.(check bool) "different dk" true (dk1 <> dk2);
  (* Each keypair should work correctly *)
  (match ML_KEM_768.encapsulate ek1 with
   | Error msg -> Alcotest.fail ("Encapsulation with ek1 failed: " ^ msg)
   | Ok (ct1, ss1) ->
     match ML_KEM_768.decapsulate dk1 ct1 with
     | Error msg -> Alcotest.fail ("Decapsulation with dk1 failed: " ^ msg)
     | Ok ss1' ->
       Alcotest.(check string) "keypair 1 works" ss1 ss1');
  (match ML_KEM_768.encapsulate ek2 with
   | Error msg -> Alcotest.fail ("Encapsulation with ek2 failed: " ^ msg)
   | Ok (ct2, ss2) ->
     match ML_KEM_768.decapsulate dk2 ct2 with
     | Error msg -> Alcotest.fail ("Decapsulation with dk2 failed: " ^ msg)
     | Ok ss2' ->
       Alcotest.(check string) "keypair 2 works" ss2 ss2')

let test_cross_keypair_failure () =
  (* Using wrong dk should not produce the correct shared secret *)
  let ek1, _dk1 = ML_KEM_768.generate () in
  let _ek2, dk2 = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek1 with
  | Error msg -> Alcotest.fail ("Encapsulation failed: " ^ msg)
  | Ok (ct, ss1) ->
    (* Try to decapsulate with wrong dk *)
    match ML_KEM_768.decapsulate dk2 ct with
    | Error msg -> Alcotest.fail ("Decapsulation should not error: " ^ msg)
    | Ok ss2 ->
      (* Should produce output but it should be different *)
      Alcotest.(check bool) "cross-keypair produces different ss" true (ss1 <> ss2)

let test_deterministic_decapsulation () =
  (* Same ciphertext should always produce same shared secret *)
  let ek, dk = ML_KEM_768.generate () in
  match ML_KEM_768.encapsulate ek with
  | Error msg -> Alcotest.fail ("Encapsulation failed: " ^ msg)
  | Ok (ct, ss) ->
    (* Decapsulate multiple times *)
    for _ = 1 to 10 do
      match ML_KEM_768.decapsulate dk ct with
      | Error msg -> Alcotest.fail ("Decapsulation failed: " ^ msg)
      | Ok ss' ->
        Alcotest.(check string) "deterministic decapsulation" ss ss'
    done

let tests = [
  ("key sizes", `Quick, test_key_sizes);
  ("keygen", `Quick, test_keygen);
  ("encaps/decaps", `Quick, test_encaps_decaps);
  ("invalid ek length", `Quick, test_invalid_ek_length);
  ("invalid dk length", `Quick, test_invalid_dk_length);
  ("invalid ct length", `Quick, test_invalid_ct_length);
  ("multiple encaps", `Quick, test_multiple_encaps);
  ("implicit rejection", `Quick, test_implicit_rejection);
  ("multiple keypairs", `Quick, test_multiple_keypairs);
  ("cross keypair failure", `Quick, test_cross_keypair_failure);
  ("deterministic decapsulation", `Quick, test_deterministic_decapsulation);
]

let () = Alcotest.run "ML-KEM-768" [ "ML_KEM_768", tests ]
