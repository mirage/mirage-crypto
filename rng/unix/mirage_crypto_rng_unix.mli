(** {b RNG} seeding on {b Unix}.

    This module provides the RNG [Getrandom] which calls [getrandom ()] for each
    [generate] request. On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()]
    is used instead.

    The RNG [Devrandom] opens [/dev/urandom] (or [/dev/random] if the former is
    absent) at initialization time, and reads random bytes from that file
    descriptor.

    Calling {{!initialize}initialize} is enough to bring the RNG into a working
    state.

    [initialize] is idempotent as long as the default generator is unchanged.
    It is harmless to call it several times.
*)

module Getrandom : Mirage_crypto_rng.Generator

(*module Devrandom : Mirage_crypto_rng.Generator *)

val initialize : unit -> unit
