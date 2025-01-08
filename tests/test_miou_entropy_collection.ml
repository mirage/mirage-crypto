module Printing_rng = struct
  type g = unit

  let block = 16
  let create ?time:_ () = ()
  let generate_into ~g:_ _buf ~off:_ _len = assert false
  let seeded ~g:_ = true
  let pools = 1

  let reseed ~g:_ data =
    Format.printf "reseeding:@.%a@.%!" (Ohex.pp_hexdump ()) data

  let accumulate ~g:_ source =
    let print data =
      Format.printf "accumulate: (src: %a) %a@.%!"
        Mirage_crypto_rng.Entropy.pp_source source Ohex.pp data
    in
    `Acc print
end

let () =
  Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.initialize (module Printing_rng) in
  Format.printf "entropy sources: %a@,%!"
    (fun ppf -> List.iter (fun x ->
         Mirage_crypto_rng.Entropy.pp_source ppf x;
         Format.pp_print_space ppf ()))
    (Mirage_crypto_rng.Entropy.sources ());
  let sleep = Duration.(of_sec 2 |> to_f) in
  Miou_unix.sleep sleep;
  Mirage_crypto_rng_miou_unix.kill rng
