(** {b RNG} seeding on {b Mkernel}. *)

type rng
(** Type of tasks seeding the RNG. *)

val initialize : ?g:'a -> ?sleep:int -> 'a Mirage_crypto_rng.generator -> rng
(** [initialize ~g ~sleep generator] sets the default generator to the [generator] and sets up periodic entropy feeding for that rng. The argument [sleep] (default: 1 second) is measured in nanoseconds, and is the wait between two CPU-assisted entropy collection (using RDRAND).

Raises [Invalid_argument] if called a second time.

val kill : rng -> unit
(** [kill rng] cancels the periodic seeding and unsets the default random number generator. *)
