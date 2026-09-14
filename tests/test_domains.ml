open OUnit2
open Mirage_crypto

let invalid_nonce_message =
  "Mirage_crypto: GCM: invalid nonce of length 0"

let reject_invalid_nonce () =
  let key = AES.GCM.of_secret (String.make 16 '\x00') in
  try
    ignore (AES.GCM.authenticate_encrypt ~key ~nonce:"" "");
    assert_failure "AES-GCM accepted an empty nonce"
  with Invalid_argument message -> message

let invalid_arguments_are_domain_local _ =
  let spawned_message = Domain.(spawn reject_invalid_nonce |> join) in
  let main_message = reject_invalid_nonce () in
  let printer (spawned, main) =
    Printf.sprintf "(spawned: %S, main: %S)" spawned main
  in
  assert_equal ~printer ~msg:"rejection messages"
    (invalid_nonce_message, invalid_nonce_message)
    (spawned_message, main_message)

let () =
  run_test_tt_main
    ("Domains" >::: [
         "invalid arguments are domain-local"
         >:: invalid_arguments_are_domain_local;
       ])
