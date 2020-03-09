(** {b RNG} seeding on {b Unix}.

    This module provides the RNG [Getrandom] which calls [getrandom ()] for each
    [generate] request. On BSD systems (FreeBSD, OpenBSD, macOS) [getentropy ()]
    is used instead.

    Calling {{!initialize}initialize} is enough to bring the RNG into a working
    state.

    [initialize] is idempotent as long as the default generator is unchanged.
    It is harmless to call it several times.
*)

module Getrandom : Mirage_crypto_rng.Generator

val initialize : unit -> unit
