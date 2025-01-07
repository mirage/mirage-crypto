
external getrandom_buf : bytes -> int -> int -> unit = "mc_getrandom" [@@noalloc]

type g = unit

(* The maximum value for length is GETENTROPY_MAX for `getentropy`: https://pubs.opengroup.org/onlinepubs/9799919799/functions/getentropy.html
    The minimum acceptable value for GETENTROPY_MAX is 256 https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/limits.h.html

    The actual implementation may be one of `getrandom`, `getentropy`, or `BCryptGenRandom`, and will internally limit the maximum bytes read in one go and loop as needed if more bytes are requested and we get a short read.
  *)
let block = 256

let create ?time:_ () = ()

let generate_into ~g:_ buf ~off len =
  getrandom_buf buf off len

let reseed ~g:_ _data = ()

let accumulate ~g:_ _source =
  `Acc (fun _data -> ())

let seeded ~g:_ = true

let pools = 0
