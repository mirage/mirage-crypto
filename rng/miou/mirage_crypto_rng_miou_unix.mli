(** {b RNG} seeding on {b Miou_unix}.

    This module initializes a RNG with [getrandom()], and CPU RNG. On BSD system
    (FreeBSD, OpenBSD, MacOS) [getentropy()] is used instead of [getrandom()].
    On Windows 10 or higher, [BCryptGenRandom()] is used with the default RNG.
    Windows 8 or lower are not supported by this library.
*)

module Pfortuna : Mirage_crypto_rng.Generator
(** {b Pfortuna}, a {b domain-safe} CSPRNG
    {{: https://www.schneier.com/fortuna.html} proposed} by Schneier. *)

type rng
(** Type of tasks seeding the RNG. *)

val initialize : ?g:'a -> ?sleep:int64 -> 'a Mirage_crypto_rng.generator -> rng
[@@deprecated "Use 'Mirage_crypto_rng_unix.use_default ()' instead."]
(** [initialize ?g ?sleep (module Generator)] will allow the RNG to operate in a
    returned task. This task periodically launches sub-tasks that seed the
    engine (using [getrandom()], [getentropy()] or [BCryptGenRandom()] depending
    on the system). These sub-tasks must be cleaned periodically (in seconds)
    according to the [sleep] parameter given (defaults to 1 second).

    The user must then {!val:kill} the returned task at the end of the program
    to be sure to clean everything. Otherwise, Miou will complain with the
    exception [Still_has_children].

    We strongly recommend using {!module:Pfortuna} as an RNG engine rather than
    {!module:Mirage_crypto_rng.Fortuna}. The engine is launched in parallel with
    the other tasks if at least one domain is available. To ensure that there is
    no compromise in the values generated by a {i data-race}, [Pfortuna] is an
    {b domain-safe} implementation of Fortuna.

    The user cannot make any subsequent calls to [initialize]. In other words,
    you can only initialise a single {!type:rng} task. You must {!val:kill} the
    returned {!type:rng} if you want to re-initialise the RNG.

    A basic usage of [mirage-crypto-rng-miou-unix] is:
    {[
      let () = Miou_unix.run @@ fun () ->
        let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
        let str = Mirage_crypto_rng.generate 16 in
        Format.printf "random: %S\n%!" str;
        Mirage_crypto_rng_miou_unix.kill rng
    ]} *)

val kill : rng -> unit
(** [kill rng] terminates the {i background} task which seeds the RNG. *)
