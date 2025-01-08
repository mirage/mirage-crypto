let () = Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let random_num = Mirage_crypto_rng.generate 32 in
  assert (String.length random_num = 32);
  Printf.printf "32 bit random number: %s\n%!" (Ohex.encode random_num);
  let random_num = Mirage_crypto_rng.generate 16 in
  assert (String.length random_num = 16);
  Printf.printf "16 bit random number: %s\n%!" (Ohex.encode random_num);
  (* NOTE(dinosaure): the test below shows that [Pfortuna] is domain-safe when
     run with TSan. If we use the Fortuna engine, TSan will report invalid
     accesses between the domain that seeds the RNG and [dom0]. *)
  for _ = 0 to 4 do
    let _ = Mirage_crypto_rng.generate 16 in
    Miou_unix.sleep 0.5;
  done;
  Mirage_crypto_rng_miou_unix.kill rng
