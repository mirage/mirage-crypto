open Lwt.Infix

module Printing_rng = struct
  type g = unit

  let block = 16

  let create () = ()

  let generate ~g:_ _n = assert false

  let reseed ~g:_ data =
    Format.printf "reseeding: %a@.%!" Cstruct.hexdump_pp data

  let accumulate ~g:_ ~source =
    let print data =
      Format.printf "accumulate: (src:%d) %a@.%!" source Cstruct.hexdump_pp data
    in
    `Acc print

  let seeded ~g:_ = true
end

module E = Mirage_crypto_entropy.Make(Time)

let with_entropy act =
  E.initialize (module Printing_rng) >>= fun _ ->
  Format.printf "entropy sources: %a@,%!"
    (fun ppf -> List.iter (fun x ->
         Mirage_crypto_entropy.pp_source ppf x;
         Format.pp_print_space ppf ()))
    (Mirage_crypto_entropy.sources ());
  act ()

let () =
  OS.(Main.run (with_entropy (fun () -> Time.sleep_ns 1_000L)))
