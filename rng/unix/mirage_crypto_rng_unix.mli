(** {b RNG} seeding on {b Unix}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
    On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()] is used instead
    of [getrandom ()]. On Windows 10 or higher, [BCryptGenRandom()] is used
    with the default RNG. Windows 8 or lower are not supported by this library.
*)

(** [initialize ~g rng] will bring the RNG into a working state. *)
val initialize : ?g:'a -> 'a Mirage_crypto_rng.generator -> unit
[@@deprecated "Use 'Mirage_crypto_rng_unix.use_default ()' instead."]

(** [getrandom size] returns a buffer of [size] filled with random bytes. *)
val getrandom : int -> string

(** A generator that opens /dev/urandom and reads from that file descriptor
    data whenever random data is needed. The file descriptor is closed in
    [at_exit]. *)
module Urandom : Mirage_crypto_rng.Generator

(** A generator using [getrandom(3)] on Linux, [getentropy(3)] on BSD and macOS,
    and [BCryptGenRandom()] on Windows. *)
module Getentropy : Mirage_crypto_rng.Generator

(** [use_default ()] initializes the RNG [Mirage_crypto_rng.default_generator]
    with a sensible default, at the moment using [Getentropy]. *)
val use_default : unit -> unit

(** [use_dev_random ()] initializes the RNG
    [Mirage_crypto_rng.default_generator] with the [Urandom] generator. This
    raises an exception if "/dev/urandom" cannot be opened. *)
val use_dev_urandom : unit -> unit

(** [use_getentropy ()] initializes the RNG [Mirage_crypto_rng.default_generator]
    with the [Getentropy] generator. *)
val use_getentropy : unit -> unit
