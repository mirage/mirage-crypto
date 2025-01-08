open Mirage_crypto_rng

let () =
  Eio_main.run @@ fun env ->
  begin[@alert "-deprecated"]
    Mirage_crypto_rng_eio.run (module Fortuna) env @@ fun () ->
    let random_num = Mirage_crypto_rng.generate 32 in
    assert (String.length random_num = 32);
    Printf.printf "32 bit random number: %S\n%!" random_num;
    let random_num = Mirage_crypto_rng.generate 16 in
    assert (String.length random_num = 16);
    Printf.printf "16 bit random number: %S\n%!" random_num;
  end
