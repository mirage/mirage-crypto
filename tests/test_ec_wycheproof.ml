open Wycheproof

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

let parse_asn1 curve s =
  let cs = Cstruct.of_string s in
  let seq2 a b = Asn.S.(sequence2 (required a) (required b)) in
  let term = Asn.S.(seq2 (seq2 oid oid) bit_string_cs) in
  let ec_public_key = Asn.OID.(base 1 2 <|| [ 840; 10045; 2; 1 ]) in
  let prime_oid = match curve with
    | "secp224r1" -> Asn.OID.(base 1 3 <|| [ 132; 0; 33 ])
    | "secp256r1" -> Asn.OID.(base 1 2 <|| [ 840; 10045; 3; 1; 7 ])
    | "secp384r1" -> Asn.OID.(base 1 3 <|| [ 132; 0; 34 ])
    | "secp521r1" -> Asn.OID.(base 1 3 <|| [ 132; 0; 35 ])
    | _ -> assert false
  in
  match Asn.decode (Asn.codec Asn.ber term) cs with
  | Error _ -> Error "ASN1 parse error"
  | Ok (((oid1, oid2), data), rest) ->
      if Cstruct.len rest <> 0 then Error "ASN1 leftover"
      else if not (Asn.OID.equal oid1 ec_public_key) then
        Error "ASN1: wrong oid 1"
      else if not (Asn.OID.equal oid2 prime_oid) then Error "ASN1: wrong oid 2"
      else Ok (Cstruct.to_string data)

let ( >>= ) xr f = match xr with Error _ as e -> e | Ok x -> f x

let parse_point curve p =
  parse_asn1 curve p >>= fun h ->
  Ok Hex.(to_cstruct (of_string h))

let to_string_result ~pp_error = function
  | Ok _ as ok -> ok
  | Error e ->
      let msg = Format.asprintf "%a" pp_error e in
      Error msg

let pad ~total_len cs =
  match total_len - Cstruct.len cs with
  | 0 -> Ok cs
  | n when n < 0 ->
    let is_zero = ref true in
    for i = 0 to abs n - 1 do
      if Cstruct.get_uint8 cs i <> 0 then
        is_zero := false
    done;
    if !is_zero then
      Ok (Cstruct.sub cs (abs n) total_len)
    else
      Error "input is too long"
  | pad_len -> Ok (Cstruct.append (Cstruct.create pad_len) cs)

let len = function
  | "secp224r1" -> 28
  | "secp256r1" -> 32
  | "secp384r1" -> 48
  | "secp521r1" -> 66
  | _ -> assert false

let parse_secret curve s =
  let total_len = len curve in
  pad ~total_len (Cstruct.of_string s)

type test = {
  public_key : Cstruct.t;
  raw_private_key : Cstruct.t;
  expected : string;
}

let perform_key_exchange curve ~public_key ~raw_private_key =
  let open Mirage_crypto_ec in
  let rng _ = raw_private_key in
  to_string_result ~pp_error
    (match curve with
     | "secp224r1" ->
       P224.Dh.key_exchange (fst (P224.Dh.gen_key ~rng)) public_key
     | "secp256r1" ->
       P256.Dh.key_exchange (fst (P256.Dh.gen_key ~rng)) public_key
     | "secp384r1" ->
       P384.Dh.key_exchange (fst (P384.Dh.gen_key ~rng)) public_key
     | "secp521r1" ->
       P521.Dh.key_exchange (fst (P521.Dh.gen_key ~rng)) public_key
     | _ -> assert false)

let interpret_test ~tcId curve { public_key; raw_private_key; expected } () =
  match perform_key_exchange curve ~public_key ~raw_private_key with
  | Ok cs ->
      let got = Cstruct.to_string cs in
      Alcotest.check hex __LOC__ expected got
  | Error err -> Printf.ksprintf Alcotest.fail "While parsing %d: %s" tcId err

type invalid_test = { public : string; private_ : string }

let is_ok = function Ok _ -> true | Error _ -> false

let interpret_invalid_test curve { public; private_ } () =
  let result =
    parse_point curve public >>= fun public_key ->
    parse_secret curve private_ >>= fun raw_private_key ->
    perform_key_exchange curve ~public_key ~raw_private_key
  in
  Alcotest.check Alcotest.bool __LOC__ false (is_ok result)

type strategy = Test of test | Invalid_test of invalid_test | Skip

let make_ecdh_test curve (test : ecdh_test) =
  let ignored_flags = [ "CompressedPoint"; "UnnamedCurve" ] in
  match test.result with
  | _ when has_ignored_flag test ~ignored_flags -> Ok Skip
  | Invalid ->
      Ok (Invalid_test { public = test.public; private_ = test.private_ })
  | Acceptable -> Ok Skip
  | Valid ->
    parse_point curve test.public >>= fun public_key ->
    parse_secret curve test.private_ >>= fun raw_private_key ->
    Ok (Test { public_key; raw_private_key; expected = test.shared })

let concat_map f l = List.map f l |> List.concat

let to_ecdh_tests curve (x : ecdh_test) =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  match make_ecdh_test curve x with
  | Ok (Test t) -> [ (name, `Quick, interpret_test ~tcId:x.tcId curve t) ]
  | Ok (Invalid_test t) -> [ (name, `Quick, interpret_invalid_test curve t) ]
  | Ok Skip -> []
  | Error e -> Printf.ksprintf failwith "While parsing %d: %s" x.tcId e

let ecdh_tests file =
  let data = load_file_exn file in
  let groups : ecdh_test_group list =
    List.map ecdh_test_group_exn data.testGroups
  in
  concat_map (fun (group : ecdh_test_group) ->
      concat_map (to_ecdh_tests group.curve) group.tests)
    groups

let parse_sig size cs =
  let asn = Asn.S.(sequence2 (required integer) (required integer)) in
  match Asn.(decode (codec der asn) cs) with
  | Error _ -> Error "ASN1 parse error"
  | Ok ((r, s), rest) ->
    if Cstruct.len rest <> 0 then Error "ASN1 leftover"
    else
      let check_size y =
        let bytes x = 1 + ((x - 1) / 8) in
        if bytes (Z.numbits y) > size then
          Error "signature too long"
        else
          Ok ()
      in
      check_size r >>= fun () ->
      check_size s >>= fun () ->
      if Z.sign r < 0 || Z.sign s < 0 then
        Error "r or s must be > 0"
      else
        Ok (Mirage_crypto_pk.Z_extra.to_cstruct_be ~size r,
            Mirage_crypto_pk.Z_extra.to_cstruct_be ~size s)

let make_ecdsa_test curve key hash (tst : ecdsa_test) =
  let name = Printf.sprintf "%d - %s" tst.tcId tst.comment in
  let msg = Mirage_crypto.Hash.digest hash (Cstruct.of_string tst.msg) in
  let size = len curve in
  let verified (r,s) =
    match curve with
    | "secp224r1" ->
      begin match Mirage_crypto_ec.P224.Dsa.pub_of_cstruct key with
        | Ok key -> Mirage_crypto_ec.P224.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp256r1" ->
      begin match Mirage_crypto_ec.P256.Dsa.pub_of_cstruct key with
        | Ok key -> Mirage_crypto_ec.P256.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp384r1" ->
      begin match Mirage_crypto_ec.P384.Dsa.pub_of_cstruct key with
        | Ok key -> Mirage_crypto_ec.P384.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp521r1" ->
      begin match Mirage_crypto_ec.P521.Dsa.pub_of_cstruct key with
        | Ok key -> Mirage_crypto_ec.P521.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | _ -> assert false
  in
  match tst.result with
  | Acceptable
  | Invalid ->
    let f () =
      match parse_sig size (Cstruct.of_string tst.sig_) with
      | Ok (r, s) -> Alcotest.(check bool __LOC__ false (verified (r, s)))
      | Error _s -> ()
    in
    name, `Quick, f
  | Valid ->
    let f () =
      match parse_sig size (Cstruct.of_string tst.sig_) with
      | Ok (r, s) -> Alcotest.(check bool __LOC__ true (verified (r, s)))
      | Error s -> Alcotest.fail s
    in
    name, `Quick, f

let to_ecdsa_tests (x : ecdsa_test_group) =
  let hash = match x.sha with
    | "SHA-256" -> `SHA256
    | "SHA-384" -> `SHA384
    | "SHA-512" -> `SHA512
    | "SHA-224" -> `SHA224
    | _ -> assert false
  in
  List.map (make_ecdsa_test x.key.curve (Cstruct.of_string x.key.uncompressed) hash) x.tests

let ecdsa_tests file =
  let data = load_file_exn file in
  let groups : ecdsa_test_group list =
    List.map ecdsa_test_group_exn data.testGroups
  in
  concat_map to_ecdsa_tests groups

let () =
  Alcotest.run "Wycheproof NIST curves" [
    ("ECDH P224 test vectors", ecdh_tests "ecdh_secp224r1_test.json") ;
    ("ECDSA P224 test vectors (SHA224)", ecdsa_tests "ecdsa_secp224r1_sha224_test.json") ;
    ("ECDSA P224 test vectors (SHA256)", ecdsa_tests "ecdsa_secp224r1_sha256_test.json") ;
    ("ECDSA P224 test vectors (SHA512)", ecdsa_tests "ecdsa_secp224r1_sha512_test.json") ;
    ("ECDH P256 test vectors", ecdh_tests "ecdh_secp256r1_test.json") ;
    ("ECDSA P256 test vectors (SHA256)", ecdsa_tests "ecdsa_secp256r1_sha256_test.json") ;
    ("ECDSA P256 test vectors (SHA512)", ecdsa_tests "ecdsa_secp256r1_sha512_test.json") ;
    ("ECDH P384 test vectors", ecdh_tests "ecdh_secp384r1_test.json") ;
    ("ECDSA P384 test vectors (SHA384)", ecdsa_tests "ecdsa_secp384r1_sha384_test.json") ;
    ("ECDSA P384 test vectors (SHA512)", ecdsa_tests "ecdsa_secp384r1_sha512_test.json") ;
    ("ECDH P521 test vectors", ecdh_tests "ecdh_secp521r1_test.json") ;
    ("ECDSA P521 test vectors (SHA512)", ecdsa_tests "ecdsa_secp521r1_sha512_test.json")
  ]
