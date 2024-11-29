(** {b RNG} seeding on {b Unix}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
    On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()] is used instead
    of [getrandom ()]. On Windows 10 or higher, [BCryptGenRandom()] is used
    with the default RNG. Windows 8 or lower are not supported by this library.
*)

(** [initialize ~g rng] will bring the RNG into a working state. *)
val initialize : ?g:'a -> 'a Mirage_crypto_rng.generator -> unit

(** [getrandom size] returns a buffer of [size] filled with random bytes. *)
val getrandom : int -> string

(** [getrandom_into buf ~off ~len] fills [buf] with random data ([len] octets),
    starting at [off]. *)
val getrandom_into : bytes -> off:int -> len:int -> unit

module Urandom : Mirage_crypto_rng.Generator

module Getentropy : Mirage_crypto_rng.Generator

val use_default : unit -> unit
(** [use_default ()] initializes with [Urandom] or resorts to [Getentropy] otherwise. *)

val use_dev_urandom : unit -> unit

val use_getentropy : unit -> unit
