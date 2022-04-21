let () = 
  Eio_main.run @@ fun env -> 
  Mirage_crypto_rng_eio.run env @@ fun () ->
  let random_num = Mirage_crypto_rng.generate 32 in 
  assert (Cstruct.length random_num = 32);
  Printf.printf "Random number: %S%!" (Cstruct.to_string random_num)
