open Wycheproof

open Mirage_crypto_ec

let ( let* ) = Result.bind

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

module Asn = struct
  let parse_point curve s =
    let seq2 a b = Asn.S.(sequence2 (required a) (required b)) in
    let term = Asn.S.(seq2 (seq2 oid oid) bit_string_octets) in
    let ec_public_key = Asn.OID.(base 1 2 <|| [ 840; 10045; 2; 1 ]) in
    let prime_oid = match curve with
      | "secp256r1" -> Asn.OID.(base 1 2 <|| [ 840; 10045; 3; 1; 7 ])
      | "secp384r1" -> Asn.OID.(base 1 3 <|| [ 132; 0; 34 ])
      | "secp521r1" -> Asn.OID.(base 1 3 <|| [ 132; 0; 35 ])
      | _ -> assert false
    in
    match Asn.decode (Asn.codec Asn.ber term) s with
    | Error _ -> Error "ASN1 parse error"
    | Ok (((oid1, oid2), data), rest) ->
      if String.length rest <> 0 then Error "ASN1 leftover"
      else if not (Asn.OID.equal oid1 ec_public_key) then
        Error "ASN1: wrong oid 1"
      else if not (Asn.OID.equal oid2 prime_oid) then Error "ASN1: wrong oid 2"
      else Ok data

  let parse_signature cs =
    let asn = Asn.S.(sequence2 (required unsigned_integer) (required unsigned_integer)) in
    match Asn.(decode (codec der asn) cs) with
    | Error _ -> Error "ASN1 parse error"
    | Ok (r_s, rest) ->
      if String.length rest <> 0 then Error "ASN1 leftover"
      else
        Ok r_s
end

let to_string_result ~pp_error = function
  | Ok _ as ok -> ok
  | Error e ->
      let msg = Format.asprintf "%a" pp_error e in
      Error msg

let pad ~total_len buf =
  match total_len - String.length buf with
  | 0 -> Ok buf
  | n when n < 0 ->
    let is_zero = ref true in
    for i = 0 to abs n - 1 do
      if Bytes.(get_uint8 (Bytes.unsafe_of_string buf) i) <> 0 then
        is_zero := false
    done;
    if !is_zero then
      Ok (String.sub buf (abs n) total_len)
    else
      Error "input is too long"
  | pad_len ->
    Ok (String.make pad_len '\000' ^ buf)

let len = function
  | "secp256r1" -> 32
  | "secp384r1" -> 48
  | "secp521r1" -> 66
  | _ -> assert false

let parse_secret curve s =
  let total_len = len curve in
  pad ~total_len s

type test = {
  public_key : string;
  raw_private_key : string;
  expected : string;
}

let perform_key_exchange curve ~public_key ~raw_private_key =
  to_string_result ~pp_error
    (match curve with
     | "secp256r1" ->
       begin match P256.Dh.secret_of_octets raw_private_key with
         | Ok (p, _) -> P256.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | "secp384r1" ->
       begin match P384.Dh.secret_of_octets raw_private_key with
         | Ok (p, _) -> P384.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | "secp521r1" ->
       begin match P521.Dh.secret_of_octets raw_private_key with
         | Ok (p, _) -> P521.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | _ -> assert false)

let interpret_test ~tcId curve { public_key; raw_private_key; expected } () =
  match perform_key_exchange curve ~public_key ~raw_private_key with
  | Ok got -> Alcotest.check hex __LOC__ expected got
  | Error err ->
    Printf.ksprintf (fun s -> Alcotest.fail s) "While parsing %d: %s" tcId err

type invalid_test = { public : string; private_ : string }

let is_ok = function Ok _ -> true | Error _ -> false

let interpret_invalid_test curve { public; private_ } () =
  let result =
    let* public_key = Asn.parse_point curve public in
    let* raw_private_key = parse_secret curve private_ in
    perform_key_exchange curve ~public_key ~raw_private_key
  in
  Alcotest.check Alcotest.bool __LOC__ false (is_ok result)

type strategy = Test of test | Invalid_test of invalid_test | Skip

let make_ecdh_test curve (test : ecdh_test) =
  let ignored_flags = ["UnnamedCurve"] in
  let curve_compression_test curve =
    let curves = ["secp256r1"; "secp384r1"; "secp521r1"] in
    test.tcId = 2 && List.exists (fun x -> String.equal x curve) curves
  in
  match test.result with
  | _ when has_ignored_flag test ~ignored_flags -> Ok Skip
  | Invalid ->
      Ok (Invalid_test { public = test.public; private_ = test.private_ })
  | Acceptable when curve_compression_test curve ->
    let* public_key = Asn.parse_point curve test.public in
    let* raw_private_key = parse_secret curve test.private_ in
    Ok (Test { public_key; raw_private_key; expected = test.shared })
  | Acceptable -> Ok Skip
  | Valid ->
    let* public_key = Asn.parse_point curve test.public in
    let* raw_private_key = parse_secret curve test.private_ in
    Ok (Test { public_key; raw_private_key; expected = test.shared })

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
  List.concat_map (fun (group : ecdh_test_group) ->
      List.concat_map (to_ecdh_tests group.curve) group.tests)
    groups

let make_ecdsa_test curve key hash (tst : dsa_test) =
  let name = Printf.sprintf "%d - %s" tst.tcId tst.comment in
  let size = len curve in
  let msg =
    let dgst =
      match hash with
      | "SHA-256" -> Digestif.SHA256.(digest_string tst.msg |> to_raw_string)
      | "SHA-384" -> Digestif.SHA384.(digest_string tst.msg |> to_raw_string)
      | "SHA-512" -> Digestif.SHA512.(digest_string tst.msg |> to_raw_string)
      | "SHA-224" -> Digestif.SHA224.(digest_string tst.msg |> to_raw_string)
      | _ -> assert false
    in
    String.sub dgst 0 (min size (String.length dgst))
  in
  let verified (r,s) =
    match curve with
    | "secp256r1" ->
      begin match P256.Dsa.pub_of_octets key with
        | Ok key -> P256.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp384r1" ->
      begin match P384.Dsa.pub_of_octets key with
        | Ok key -> P384.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp521r1" ->
      begin match P521.Dsa.pub_of_octets key with
        | Ok key -> P521.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | _ -> assert false
  in
  match tst.result with
  | Acceptable
  | Invalid ->
    let f () =
      match Asn.parse_signature tst.sig_ with
      | Ok (r, s) -> Alcotest.(check bool __LOC__ false (verified (r, s)))
      | Error _s -> ()
    in
    name, `Quick, f
  | Valid ->
    let f () =
      match Asn.parse_signature tst.sig_ with
      | Ok (r, s) -> Alcotest.(check bool __LOC__ true (verified (r, s)))
      | Error s -> Alcotest.fail s
    in
    name, `Quick, f

let to_ecdsa_tests (x : ecdsa_test_group) =
  List.map
    (make_ecdsa_test x.key.curve x.key.uncompressed x.sha)
    x.tests

let ecdsa_tests file =
  let data = load_file_exn file in
  let groups : ecdsa_test_group list =
    List.map ecdsa_test_group_exn data.testGroups
  in
  List.concat_map to_ecdsa_tests groups

let to_x25519_test (x : ecdh_test) =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment
  and priv =
    match X25519.secret_of_octets x.private_ with
    | Ok (p, _) -> p
    | Error _ -> assert false
  in
  match x.result with
  | Acceptable ->
    let f () =
      match
        X25519.key_exchange priv x.public,
        has_ignored_flag x ~ignored_flags:[ "LowOrderPublic" ]
      with
      | Ok _, true -> Alcotest.fail "acceptable should have errored"
      | Ok r, false ->
        Alcotest.(check bool __LOC__ true (String.equal r x.shared))
      | Error _, true -> ()
      | Error e, false -> Alcotest.failf "acceptable errored %a" pp_error e
    in
    name, `Quick, f
  | Invalid ->
    let f () =
      match X25519.key_exchange priv x.public with
      | Ok r -> Alcotest.(check bool __LOC__ false (String.equal r x.shared))
      | Error e -> Alcotest.failf "invalid errored %a" pp_error e
    in
    name, `Quick, f
  | Valid ->
    let f () =
      match X25519.key_exchange priv x.public with
      | Ok r -> Alcotest.(check bool __LOC__ true (String.equal r x.shared))
      | Error e -> Alcotest.failf "valid errored %a" pp_error e
    in
    name, `Quick, f

let x25519_tests =
  let data = load_file_exn "x25519_test.json" in
  let groups : ecdh_test_group list =
    List.map ecdh_test_group_exn data.testGroups
  in
  List.concat_map (fun (group : ecdh_test_group) ->
      List.map to_x25519_test group.tests)
    groups

let to_ed25519_test (priv, pub) (x : dsa_test) =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  match x.result with
  | Invalid ->
    let f () =
      Alcotest.(check bool __LOC__ false (Ed25519.verify ~key:pub x.sig_ ~msg:x.msg));
      let s = Ed25519.sign ~key:priv x.msg in
      Alcotest.(check bool __LOC__ false (String.equal s x.sig_))
    in
    name, `Quick, f
  | Valid ->
    let f () =
      Alcotest.(check bool __LOC__ true (Ed25519.verify ~key:pub x.sig_ ~msg:x.msg));
      let s = Ed25519.sign ~key:priv x.msg in
      Alcotest.(check bool __LOC__ true (String.equal s x.sig_))
    in
    name, `Quick, f
  | Acceptable -> assert false

let to_ed25519_keys (key : eddsa_key) =
  match Ed25519.priv_of_octets key.sk, Ed25519.pub_of_octets key.pk with
  | Ok priv, Ok pub ->
    assert (String.equal Ed25519.(pub_to_octets (pub_of_priv priv)) key.pk);
    priv, pub
  | _ -> assert false

let ed25519_tests =
  let data = load_file_exn "eddsa_test.json" in
  let groups : eddsa_test_group list =
    List.map eddsa_test_group_exn data.testGroups
  in
  List.concat_map (fun (group : eddsa_test_group) ->
      let keys = to_ed25519_keys group.key in
      List.map (to_ed25519_test keys) group.tests)
    groups

let () =
  Alcotest.run "Wycheproof NIST curves" [
    ("ECDH P256 test vectors", ecdh_tests "ecdh_secp256r1_test.json") ;
    ("ECDSA P256 test vectors (SHA256)",
     ecdsa_tests "ecdsa_secp256r1_sha256_test.json") ;
    ("ECDSA P256 test vectors (SHA512)",
     ecdsa_tests "ecdsa_secp256r1_sha512_test.json") ;
    ("ECDH P384 test vectors", ecdh_tests "ecdh_secp384r1_test.json") ;
    ("ECDSA P384 test vectors (SHA384)",
     ecdsa_tests "ecdsa_secp384r1_sha384_test.json") ;
    ("ECDSA P384 test vectors (SHA512)",
     ecdsa_tests "ecdsa_secp384r1_sha512_test.json") ;
    ("ECDH P521 test vectors", ecdh_tests "ecdh_secp521r1_test.json") ;
    ("ECDSA P521 test vectors (SHA512)",
     ecdsa_tests "ecdsa_secp521r1_sha512_test.json") ;
    ("X25519 test vectors", x25519_tests) ;
    ("ED25519 test vectors", ed25519_tests) ;
  ]
