open Lwt.Infix

module Printing_rng = struct
  type g = unit

  let block = 16

  let create ?time:_ () = ()

  let generate_into ~g:_ _buf ~off:_ _len = assert false

  let reseed ~g:_ data =
    Format.printf "reseeding:@.%a@.%!" (Ohex.pp_hexdump ()) data

  let accumulate ~g:_ source =
    let print data =
      Format.printf "accumulate: (src: %a) %a@.%!"
        Mirage_crypto_rng.Entropy.pp_source source Ohex.pp data
    in
    `Acc print

  let seeded ~g:_ = true
  let pools = 1
end

let with_entropy act =
  Mirage_crypto_rng_mirage.initialize (module Printing_rng) >>= fun () ->
  Format.printf "entropy sources: %a@,%!"
    (fun ppf -> List.iter (fun x ->
         Mirage_crypto_rng.Entropy.pp_source ppf x;
         Format.pp_print_space ppf ()))
    (Mirage_crypto_rng.Entropy.sources ());
  act ()

let () =
  Unix_os.(Main.run (with_entropy (fun () -> Time.sleep_ns (Duration.of_sec 3))))
