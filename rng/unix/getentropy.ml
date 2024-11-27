
external getrandom_buf : bytes -> int -> int -> unit = "mc_getrandom" [@@noalloc]

type g = unit

let block = 256

let create ?time:_ () = ()

let generate_into ~g:_ buf ~off len =
  getrandom_buf buf off len

let reseed ~g:_ _data = ()

let accumulate ~g:_ _source =
  `Acc (fun _data -> ())

let seeded ~g:_ = true

let pools = 0
