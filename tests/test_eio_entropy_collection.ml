module Printing_rng = struct
  type g = unit

  let block = 16
  let create ?time:_ () = ()
  let generate_into ~g:_ _buf ~off:_ _len = assert false
  let seeded ~g:_ = true
  let pools = 1

  let reseed ~g:_ data =
    Format.printf "reseeding: %a@.%!" Cstruct.hexdump_pp (Cstruct.of_string data)

  let accumulate ~g:_ source =
    let print data =
      Format.printf "accumulate: (src: %a) %a@.%!"
        Mirage_crypto_rng.Entropy.pp_source source Cstruct.hexdump_pp
        (Cstruct.of_string data)
    in
    `Acc print
end

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Printing_rng) env @@ fun () ->
  Eio.Fiber.both
    begin fun () ->
       let sleep = Duration.(of_sec 2 |> to_f) in
       Eio.Time.sleep env#clock sleep
    end
    begin fun () ->
      Format.printf "entropy sources: %a@,%!"
        (fun ppf -> List.iter (fun x ->
             Mirage_crypto_rng.Entropy.pp_source ppf x;
             Format.pp_print_space ppf ()))
        (Mirage_crypto_rng.Entropy.sources ())
    end

