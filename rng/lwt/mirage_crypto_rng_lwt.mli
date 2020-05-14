(** {b RNG} seeding on {b Lwt}.

    This module initializes a Fortuna RNG with [getrandom()], and CPU RNG.
*)

(** [initialize ~sleep ()] will bring the RNG into a working state. Each [sleep]
    the CPU RNG is used to collect entropy, on every [10 * sleep] getrandom is
    used to collect entropy.
*)
val initialize : ?sleep:int64 -> unit -> unit Lwt.t
