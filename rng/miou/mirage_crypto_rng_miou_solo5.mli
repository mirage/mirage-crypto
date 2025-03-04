(** {b RNG} seeding on {b Miou_solo5}. *)

type rng
(** Type of tasks seeding the RNG. *)

val initialize : ?g:'a -> ?sleep:int -> 'a Mirage_crypto_rng.generator -> rng
val kill : rng -> unit
