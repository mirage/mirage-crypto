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

let initialize () =
  let seed =
    List.mapi (fun i f -> f i)
      Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ] |>
    Cstruct.concat
  in
  Entropy.add_source `Getrandom;
  set_default_generator (create ~seed (module Fortuna))
