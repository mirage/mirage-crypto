open OUnit2

let output_length = 32

let fork_safety _ =
  Mirage_crypto_rng_unix.use_dev_urandom ();
  ignore (Mirage_crypto_rng.generate 1);
  let input, output = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close input;
    (try
       let output_channel = Unix.out_channel_of_descr output in
       Out_channel.output_string output_channel
         (Mirage_crypto_rng.generate output_length);
       Out_channel.close output_channel;
       Unix._exit 0
     with _ -> Unix._exit 1)
  | pid ->
    Unix.close output;
    let input_channel = Unix.in_channel_of_descr input in
    let child_output =
      In_channel.really_input_string input_channel output_length
    in
    In_channel.close input_channel;
    let _, status = Unix.waitpid [] pid in
    (match status with
     | Unix.WEXITED 0 -> ()
     | _ -> assert_failure "child process failed");
    let child_output =
      match child_output with
      | Some output -> output
      | None -> assert_failure "child process returned too few random bytes"
    in
    let parent_output = Mirage_crypto_rng.generate output_length in
    assert_bool "parent and child returned the same random bytes"
      (parent_output <> child_output)

let () =
  run_test_tt_main ("urandom" >::: [ "fork safety" >:: fork_safety ])
