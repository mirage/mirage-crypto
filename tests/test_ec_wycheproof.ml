open Mirage_crypto_ec

type hex = string

let hex =
  let padded s = if String.length s mod 2 = 0 then s else "0" ^ s in
  Jsont.map ~kind:"hex" ~dec:(fun s -> Ohex.decode (padded s)) ~enc:Ohex.encode Jsont.string

type test_result = Valid | Acceptable | Invalid

let test_result_to_string = function
  | Valid -> "Valid"
  | Acceptable -> "Acceptable"
  | Invalid -> "Invalid"

let test_result_of_string = function
  | "valid" -> Valid
  | "acceptable" -> Acceptable
  | "invalid" -> Invalid
  | x -> failwith ("unknown test result: " ^ x)

let test_result =
  Jsont.map ~kind:"test result" ~dec:test_result_of_string ~enc:test_result_to_string Jsont.string

type ecdh_test = {
  tcId : int;
  comment : string;
  curve : string option;
  public : hex;
  private_ : hex;
  shared : hex;
  result : test_result;
  flags : string list;
}

let make_ecdh_test tcId comment curve public private_ shared result flags =
  { tcId ; comment ; curve ; public ; private_ ; shared ; result ; flags }

let ecdh_test =
  Jsont.Object.map ~kind:"ecdh test" make_ecdh_test
  |> Jsont.Object.mem "tcId" Jsont.int
  |> Jsont.Object.mem "comment" Jsont.string
  |> Jsont.Object.opt_mem "curve" Jsont.string
  |> Jsont.Object.mem "public" hex
  |> Jsont.Object.mem "private" hex
  |> Jsont.Object.mem "shared" hex
  |> Jsont.Object.mem "result" test_result
  |> Jsont.Object.mem "flags" Jsont.(list string)
  |> Jsont.Object.finish

let has_ignored_flag test ~ignored_flags =
  List.exists
    (fun ignored_flag -> List.mem ignored_flag test.flags)
    ignored_flags

type ecdh_test_group = {
  curve : string;
  tests : ecdh_test list;
}

let make_ecdh_test_group curve tests = { curve ; tests }

let ecdh_test_group =
  Jsont.Object.map ~kind:"ecdh test group" make_ecdh_test_group
  |> Jsont.Object.mem "curve" Jsont.string
  |> Jsont.Object.mem "tests" (Jsont.list ecdh_test)
  |> Jsont.Object.finish

let ecdh_test_file =
  Jsont.Object.map ~kind:"ecdh test file" Fun.id
  |> Jsont.Object.mem "testGroups" (Jsont.list ecdh_test_group)
  |> Jsont.Object.finish

type ecdsa_key = {
  curve : string;
  uncompressed : hex;
}

let make_ecdsa_key curve uncompressed = { curve ; uncompressed }

let ecdsa_key =
  Jsont.Object.map ~kind:"ecdsa key" make_ecdsa_key
  |> Jsont.Object.mem "curve" Jsont.string
  |> Jsont.Object.mem "uncompressed" hex
  |> Jsont.Object.finish

type dsa_test = {
  tcId : int;
  comment : string;
  msg : hex;
  sig_ : hex;
  result : test_result;
}

let make_dsa_test tcId comment msg sig_ result =
  { tcId ; comment ; msg ; sig_ ; result }

let dsa_test =
  Jsont.Object.map ~kind:"dsa test" make_dsa_test
  |> Jsont.Object.mem "tcId" Jsont.int
  |> Jsont.Object.mem "comment" Jsont.string
  |> Jsont.Object.mem "msg" hex
  |> Jsont.Object.mem "sig" hex
  |> Jsont.Object.mem "result" test_result
  |> Jsont.Object.finish

type ecdsa_test_group = {
  key : ecdsa_key;
  sha : string;
  tests : dsa_test list;
}

let make_ecdsa_test_group key sha tests = { key ; sha ; tests }

let ecdsa_test_group =
  Jsont.Object.map ~kind:"ecdsa test group" make_ecdsa_test_group
  |> Jsont.Object.mem "key" ecdsa_key
  |> Jsont.Object.mem "sha" Jsont.string
  |> Jsont.Object.mem "tests" (Jsont.list dsa_test)
  |> Jsont.Object.finish

let ecdsa_test_file =
  Jsont.Object.map ~kind:"ecdsa test file" Fun.id
  |> Jsont.Object.mem "testGroups" (Jsont.list ecdsa_test_group)
  |> Jsont.Object.finish

type eddsa_key = {
  pk : hex;
  sk : hex;
}

let make_eddsa_key pk sk = { pk ; sk }

let eddsa_key =
  Jsont.Object.map ~kind:"eddsa key" make_eddsa_key
  |> Jsont.Object.mem "pk" hex
  |> Jsont.Object.mem "sk" hex
  |> Jsont.Object.finish

type eddsa_test_group = {
  key : eddsa_key;
  tests : dsa_test list;
}

let make_eddsa_test_group key tests = { key ; tests }

let eddsa_test_group =
  Jsont.Object.map ~kind:"eddsa test group" make_eddsa_test_group
  |> Jsont.Object.mem "key" eddsa_key
  |> Jsont.Object.mem "tests" (Jsont.list dsa_test)
  |> Jsont.Object.finish

let eddsa_test_file =
  Jsont.Object.map ~kind:"eddsa test file" Fun.id
  |> Jsont.Object.mem "testGroups" (Jsont.list eddsa_test_group)
  |> Jsont.Object.finish


let ( let* ) = Result.bind

let hex = Alcotest.testable Ohex.pp String.equal

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
  match
    let* content = Bos.OS.File.read (Fpath.v file) in
    Result.map_error (fun s -> `Msg s)
      (Jsont_bytesrw.decode_string ecdh_test_file content)
  with
  | Ok groups ->
    List.concat_map (fun (group : ecdh_test_group) ->
        List.concat_map (to_ecdh_tests group.curve) group.tests)
      groups
  | Error `Msg m ->
    failwith ("error loading ECDH file " ^ file ^ ": " ^ m)

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
  match
    let* content = Bos.OS.File.read (Fpath.v file) in
    Result.map_error (fun s -> `Msg s)
      (Jsont_bytesrw.decode_string ecdsa_test_file content)
  with
  | Ok groups -> List.concat_map to_ecdsa_tests groups
  | Error `Msg m ->
    failwith ("error loading ECDSA file " ^ file ^ ": " ^ m)

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

let x25519_tests file =
  match
    let* content = Bos.OS.File.read (Fpath.v file) in
    Result.map_error (fun s -> `Msg s)
      (Jsont_bytesrw.decode_string ecdh_test_file content)
  with
  | Ok groups ->
    List.concat_map (fun (group : ecdh_test_group) ->
        List.map to_x25519_test group.tests)
      groups
  | Error `Msg m ->
    failwith ("error loading ECDH file " ^ file ^ ": " ^ m)

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

let ed25519_tests file =
  match
    let* content = Bos.OS.File.read (Fpath.v file) in
    Result.map_error (fun s -> `Msg s)
      (Jsont_bytesrw.decode_string eddsa_test_file content)
  with
  | Ok groups ->
    List.concat_map (fun (group : eddsa_test_group) ->
        let keys = to_ed25519_keys group.key in
        List.map (to_ed25519_test keys) group.tests)
      groups
  | Error `Msg m ->
    failwith ("error loading EDDSA file " ^ file ^ ": " ^ m)


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
    ("X25519 test vectors", x25519_tests "x25519_test.json") ;
    ("ED25519 test vectors", ed25519_tests "eddsa_test.json") ;
  ]
