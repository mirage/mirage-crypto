open Mirage_crypto_rng

open Stdlib.Bigarray
type buffer = (char, int8_unsigned_elt, c_layout) Array1.t
external getrandom_buf : buffer -> int -> unit = "mc_getrandom"

let getrandom size =
  let buf = Cstruct.create_unsafe size in
  getrandom_buf buf.Cstruct.buffer size;
  buf

let getrandom_init _ =
  let data = getrandom 128 in
  Entropy.header `Getrandom data

let running = ref false

let initialize () =
  if !running then
    Logs.warn
      (fun m -> m "Mirage_crypto_rng_unix.initialize has already been called, \
                   ignoring this call.")
  else begin
    (try
       let _ = default_generator () in
       Logs.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                              been set, check that this call is intentional");
     with
       No_default_generator -> ());
    running := true ;
    let seed =
      let init =
        Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ]
      in
      List.mapi (fun i f -> f i) init |> Cstruct.concat
    in
    Entropy.add_source `Getrandom;
    set_default_generator (create ~seed (module Fortuna))
  end
