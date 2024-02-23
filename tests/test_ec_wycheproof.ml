open Wycheproof

open Mirage_crypto_ec

let ( let* ) = Result.bind

let concat_map f l =
  (* adapt once OCaml 4.10 is lower bound *)
  List.map f l |> List.concat

let string_get_uint8 d off =
  (* adapt once OCaml 4.13 is lower bound *)
  Bytes.get_uint8 (Bytes.unsafe_of_string d) off

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

module Asn = struct
  (* This is a handcrafted asn1 parser, sufficient for the wycheproof tests.
     The underlying reason is to avoid a dependency on asn1-grammars and
     mirage-crypto-pk (which depends on gmp and zarith, and are cumbersome to
     build on windows with CL.EXE). *)

  let guard p e = if p then Ok () else Error e

  let decode_len start_off buf =
    let len = string_get_uint8 buf start_off in
    if len >= 0x80 then
      let bytes = len - 0x80 in
      let rec g acc off =
        if off = bytes then
          Ok (acc, bytes + start_off + 1)
        else
          let this = string_get_uint8 buf (start_off + 1 + off) in
          let* () = guard (off = 0 && this >= 0x80) "badly encoded length" in
          let acc' = acc lsl 8 + this in
          let* () = guard (acc <= acc') "decode_len overflow in acc" in
          g (acc lsl 8 + string_get_uint8 buf (start_off + 1 + off)) (succ off)
      in
      g 0 0
    else
      Ok (len, start_off + 1)

  let decode_seq data =
    let* () = guard (String.length data > 2) "decode_seq: data too short" in
    let tag = string_get_uint8 data 0 in
    let* () = guard (tag = 0x30) "decode_seq: bad tag (should be 0x30)" in
    let* len, off = decode_len 1 data in
    let* () = guard (String.length data - off >= len) "decode_seq: too short" in
    Ok (String.sub data off len,
        if String.length data - off > len then
          Some (String.sub data (off + len) (String.length data - len - off))
        else
          None)

  let decode_2_oid data =
    let decode_one off =
      let tag = string_get_uint8 data off in
      let* () = guard (tag = 0x06) "decode_oid: bad tag (should be 0x06)" in
      let len = string_get_uint8 data (off + 1) in
      let* () = guard (String.length data - 2 - off >= len) "decode_oid: data too short" in
      Ok (String.sub data (off + 2) len, off + 2 + len)
    in
    let* first, off = decode_one 0 in
    let* second, off = decode_one off in
    let* () = guard (off = String.length data) "decode_oid: leftover data" in
    Ok (first, second)

  let decode_bit_string data =
    let tag = string_get_uint8 data 0 in
    let* () = guard (tag = 0x03) "decode_bit_string: bad tag (expected 0x03)" in
    let* len, off = decode_len 1 data in
    let* () = guard (String.length data - off = len) "decode_bit_string: leftover or too short data" in
    let unused = string_get_uint8 data off in
    let* () = guard (unused = 0) "unused is not 0" in
    Ok (String.sub data (off + 1) (len - 1))

  let decode_int_pair data =
    let decode_int off =
      let* () = guard (String.length data - off > 2) "decode_int: data too short" in
      let tag = string_get_uint8 data off in
      let* () = guard (tag = 0x02) "decode_int: bad tag (should be 0x02)" in
      let len = string_get_uint8 data (off + 1) in
      let* () = guard (String.length data - off - 2 >= len) "decode_int: too short" in
      let fix_one = if string_get_uint8 data (off + 2) = 0x00 then 1 else 0 in
      let* () = guard (string_get_uint8 data (off + 2) land 0x80 = 0) "decode_int: negative number" in
      let* () =
        if String.length data > off + 3 && fix_one = 1 then
          guard (string_get_uint8 data (off + 3) <> 0x00) "decode_int: leading extra 0 byte"
        else
          Ok ()
      in
      Ok (String.sub data (fix_one + off + 2) (len - fix_one), off + len + 2)
    in
    let* first, off = decode_int 0 in
    let* second, off = decode_int off in
    let* () = guard (off = String.length data) "decode_int: leftover data" in
    Ok (first, second)

  let encode_oid = function
    | first :: second :: rt ->
      let oct1 = 40 * first + second in
      let octs = concat_map (fun x ->
          let fst = x / 16384
          and snd = x / 128
          and thr = x mod 128
          in
          assert (fst < 128);
          (if fst > 0 then [ 128 (* set high bit *) + fst ] else []) @
          (if snd > 0 then [ 128 + snd ] else []) @
          [ thr ])
          rt
      in
      String.init (1 + List.length octs) (function
          | 0 -> char_of_int oct1
          | n -> char_of_int (List.nth octs (pred n)))
    | _ -> assert false

  let parse_point curve s =
    let ec_public_key = encode_oid [ 1 ; 2 ; 840; 10045; 2; 1 ] in
    let prime_oid = encode_oid (match curve with
        | "secp224r1" -> [ 1 ; 3 ; 132; 0; 33 ]
        | "secp256r1" -> [ 1 ; 2 ; 840; 10045; 3; 1; 7 ]
        | "secp384r1" -> [ 1 ; 3 ; 132; 0; 34 ]
        | "secp521r1" -> [ 1 ; 3 ; 132; 0; 35 ]
        | _ -> assert false)
    in
    let* r = decode_seq s in
    match r with
    | _data, Some _ -> Error "expected no leftover"
    | data, None ->
      let* r = decode_seq data in
      match r with
      | _oids, None -> Error "expected some data"
      | oids, Some data ->
        let* oid1, oid2 = decode_2_oid oids in
        let* data = decode_bit_string data in
        if not (String.equal oid1 ec_public_key) then
          Error "ASN1: wrong oid 1"
        else if not (String.equal oid2 prime_oid) then
          Error "ASN1: wrong oid 2"
        else
          Ok (Cstruct.of_string data)

  let parse_signature s =
    let* r = decode_seq s in
    match r with
    | _data, Some _ -> Error "expected no leftover"
    | data, None ->
      let* r, s = decode_int_pair data in
      Ok (Cstruct.of_string r, Cstruct.of_string s)
end

let to_string_result ~pp_error = function
  | Ok _ as ok -> ok
  | Error e ->
      let msg = Format.asprintf "%a" pp_error e in
      Error msg

let pad ~total_len cs =
  match total_len - Cstruct.length cs with
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
  | pad_len ->
    Ok (Cstruct.append (Cstruct.create pad_len) cs)

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
  to_string_result ~pp_error
    (match curve with
     | "secp224r1" ->
       begin match P224.Dh.secret_of_cs raw_private_key with
         | Ok (p, _) -> P224.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | "secp256r1" ->
       begin match P256.Dh.secret_of_cs raw_private_key with
         | Ok (p, _) -> P256.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | "secp384r1" ->
       begin match P384.Dh.secret_of_cs raw_private_key with
         | Ok (p, _) -> P384.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | "secp521r1" ->
       begin match P521.Dh.secret_of_cs raw_private_key with
         | Ok (p, _) -> P521.Dh.key_exchange p public_key
         | Error _ -> assert false
       end
     | _ -> assert false)

let interpret_test ~tcId curve { public_key; raw_private_key; expected } () =
  match perform_key_exchange curve ~public_key ~raw_private_key with
  | Ok cs ->
      let got = Cstruct.to_string cs in
      Alcotest.check hex __LOC__ expected got
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
  concat_map (fun (group : ecdh_test_group) ->
      concat_map (to_ecdh_tests group.curve) group.tests)
    groups

let make_ecdsa_test curve key hash (tst : dsa_test) =
  let name = Printf.sprintf "%d - %s" tst.tcId tst.comment in
  let size = len curve in
  let msg =
    let h = Mirage_crypto.Hash.digest hash (Cstruct.of_string tst.msg) in
    Cstruct.sub h 0 (min size (Cstruct.length h))
  in
  let verified (r,s) =
    match curve with
    | "secp224r1" ->
      begin match P224.Dsa.pub_of_cstruct key with
        | Ok key -> P224.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp256r1" ->
      begin match P256.Dsa.pub_of_cstruct key with
        | Ok key -> P256.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp384r1" ->
      begin match P384.Dsa.pub_of_cstruct key with
        | Ok key -> P384.Dsa.verify ~key (r, s) msg
        | Error _ -> assert false
      end
    | "secp521r1" ->
      begin match P521.Dsa.pub_of_cstruct key with
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
  let hash = match x.sha with
    | "SHA-256" -> `SHA256
    | "SHA-384" -> `SHA384
    | "SHA-512" -> `SHA512
    | "SHA-224" -> `SHA224
    | _ -> assert false
  in
  List.map
    (make_ecdsa_test x.key.curve (Cstruct.of_string x.key.uncompressed) hash)
    x.tests

let ecdsa_tests file =
  let data = load_file_exn file in
  let groups : ecdsa_test_group list =
    List.map ecdsa_test_group_exn data.testGroups
  in
  concat_map to_ecdsa_tests groups

let to_x25519_test (x : ecdh_test) =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  let pub = Cstruct.of_string x.public
  and priv =
    match X25519.secret_of_cs (Cstruct.of_string x.private_) with
    | Ok (p, _) -> p
    | Error _ -> assert false
  and shared = Cstruct.of_string x.shared
  in
  match x.result with
  | Acceptable ->
    let f () =
      match
        X25519.key_exchange priv pub,
        has_ignored_flag x ~ignored_flags:[ "LowOrderPublic" ]
      with
      | Ok _, true -> Alcotest.fail "acceptable should have errored"
      | Ok r, false ->
        Alcotest.(check bool __LOC__ true (Cstruct.equal r shared))
      | Error _, true -> ()
      | Error e, false -> Alcotest.failf "acceptable errored %a" pp_error e
    in
    name, `Quick, f
  | Invalid ->
    let f () =
      match X25519.key_exchange priv pub with
      | Ok r -> Alcotest.(check bool __LOC__ false (Cstruct.equal r shared))
      | Error e -> Alcotest.failf "invalid errored %a" pp_error e
    in
    name, `Quick, f
  | Valid ->
    let f () =
      match X25519.key_exchange priv pub with
      | Ok r -> Alcotest.(check bool __LOC__ true (Cstruct.equal r shared))
      | Error e -> Alcotest.failf "valid errored %a" pp_error e
    in
    name, `Quick, f

let x25519_tests =
  let data = load_file_exn "x25519_test.json" in
  let groups : ecdh_test_group list =
    List.map ecdh_test_group_exn data.testGroups
  in
  concat_map (fun (group : ecdh_test_group) ->
      List.map to_x25519_test group.tests)
    groups

let to_ed25519_test (priv, pub) (x : dsa_test) =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  let msg = Cstruct.of_string x.msg
  and sig_cs = Cstruct.of_string x.sig_
  in
  match x.result with
  | Invalid ->
    let f () =
      Alcotest.(check bool __LOC__ false (Ed25519.verify ~key:pub sig_cs ~msg));
      let s = Ed25519.sign ~key:priv msg in
      Alcotest.(check bool __LOC__ false (Cstruct.equal s sig_cs))
    in
    name, `Quick, f
  | Valid ->
    let f () =
      Alcotest.(check bool __LOC__ true (Ed25519.verify ~key:pub sig_cs ~msg));
      let s = Ed25519.sign ~key:priv msg in
      Alcotest.(check bool __LOC__ true (Cstruct.equal s sig_cs))
    in
    name, `Quick, f
  | Acceptable -> assert false

let to_ed25519_keys (key : eddsa_key) =
  let priv_cs = Cstruct.of_string key.sk
  and pub_cs = Cstruct.of_string key.pk
  in
  match Ed25519.priv_of_cstruct priv_cs, Ed25519.pub_of_cstruct pub_cs with
  | Ok priv, Ok pub ->
    assert (Cstruct.equal Ed25519.(pub_to_cstruct (pub_of_priv priv)) pub_cs);
    priv, pub
  | _ -> assert false

let ed25519_tests =
  let data = load_file_exn "eddsa_test.json" in
  let groups : eddsa_test_group list =
    List.map eddsa_test_group_exn data.testGroups
  in
  concat_map (fun (group : eddsa_test_group) ->
      let keys = to_ed25519_keys group.key in
      List.map (to_ed25519_test keys) group.tests)
    groups

let () =
  Alcotest.run "Wycheproof NIST curves" [
    ("ECDH P224 test vectors", ecdh_tests "ecdh_secp224r1_test.json") ;
    ("ECDSA P224 test vectors (SHA224)",
     ecdsa_tests "ecdsa_secp224r1_sha224_test.json") ;
    ("ECDSA P224 test vectors (SHA256)",
     ecdsa_tests "ecdsa_secp224r1_sha256_test.json") ;
    ("ECDSA P224 test vectors (SHA512)",
     ecdsa_tests "ecdsa_secp224r1_sha512_test.json") ;
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
