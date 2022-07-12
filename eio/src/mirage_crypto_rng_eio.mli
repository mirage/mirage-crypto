(** {b RNG} seeding on {b Eio backends}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
    [Eio.Stdenv.secure_random] is used as the [getrandom()] implementation.
*)

type env = <
  clock: Eio.Time.clock;
  secure_random: Eio.Flow.source;
  >

(** [run ~sleep env fn] will bring the RNG into a working state. The argument
    [sleep] is measured in ns (default 1s), and used to sleep between collection
    of entropy from the CPU RNG, every [10 * sleep] getrandom is used to collect
    entropy.

    [fn] is the main function that will have access to a running RNG.

    [[
      let () =
        Eio_main.run @@ fun env ->
        Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
        let random_num = Mirage_crypto_rng.generate 32 in
        Printf.printf "Random number: %S%!" (Cstruct.to_string random_num)
    ]]
*)
val run
  :  ?g:'a
  -> ?sleep:int64
  -> 'a Mirage_crypto_rng.generator
  -> <env; ..>
  -> (unit -> 'b) -> 'b
