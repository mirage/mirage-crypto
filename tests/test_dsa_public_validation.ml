open Mirage_crypto_pk

let z = Z.of_int

let expect_invalid label ~gg ~y ~signature =
  match Dsa.pub ~p:(z 23) ~q:(z 11) ~gg:(z gg) ~y:(z y) () with
  | Error _ -> ()
  | Ok key ->
      let r, s = signature in
      if Dsa.verify ~key (String.make 1 (Char.chr r),
                          String.make 1 (Char.chr s)) "\x10"
      then failwith (label ^ ": a signature without a private key verifies")
      else failwith (label ^ ": invalid public key accepted")

let () =
  expect_invalid "identity public key" ~gg:2 ~y:1 ~signature:(2, 1);
  expect_invalid "order-two generator" ~gg:22 ~y:22 ~signature:(1, 1);
  match Dsa.pub ~p:(z 23) ~q:(z 11) ~gg:(z 2) ~y:(z 8) () with
  | Error _ -> failwith "valid DSA public key rejected"
  | Ok key ->
      if not (Dsa.verify ~key ("\x04", "\x01") "\x10") then
        failwith "valid DSA signature rejected"
