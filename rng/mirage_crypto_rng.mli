(** {1 Randomness} *)

(** Secure random number generation.

    There are several parts of this module:

    {ul
    {- The {{!Generator}signature} of generator modules, together with a
       facility to convert such modules into actual {{!g}generators}, and
       functions that operate on this representation.}
    {- A global generator instance, which needs to be initialized by calling
       {!set_default_generator}.}}
*)

(** {1 Usage notes} *)

(** {b TL;DR} Don't forget to seed; don't maintain your own [g].

    For common operations on Unix (independent of your asynchronous task
    library, you can use /dev/urandom or getentropy(3) (actually getrandom(3) on
    Linux, getentropy() on macOS and BSD systems, BCryptGenRandom on Windows).

    Please ensure to call [Mirage_crypto_rng_unix.use_default], or
    [Mirage_crypto_rng_unix.use_dev_urandom] (if you only want to use
    /dev/urandom), or [Mirage_crypto_rng_unix.use_getentropy] (if you only want
    to use getrandom/getentropy/BCryptGenRandom).

    For fine-grained control (doing entropy harvesting, etc.), please continue
    reading the documentation below. {b Please be aware that the feeding of
    Fortuna and producing random numbers is not thread-safe} (it is on Miou_unix
    via Pfortuna).

    Suitable entropy feeding of generators are provided by other libraries
    {{!Mirage_crypto_rng_mirage}mirage-crypto-rng-mirage} (for MirageOS),
    and {{!Mirage_crypto_rng_miou_unix}mirage-crypto-miou-unix} (for Miou_unix).

    The intention is that "initialize" in the respective sub-library is called
    once, which sets the default generator and registers entropy
    harvesting asynchronous tasks. The semantics is that the entropy is always
    fed to the {{!default_generator}default generator}, which is not necessarily
    the one set by "initialize". The reasoning behind this is that the default
    generator should be used in most setting, and that should be fed a constant
    stream of entropy.

    The RNGs here are merely the deterministic part of a full random number
    generation suite. For proper operation, they need to be seeded with a
    high-quality entropy source.

    Although this module exposes a more fine-grained interface, e.g. allowing
    manual seeding of generators, this is intended either for implementing
    entropy-harvesting modules, or very specialized purposes. Users of this
    library should almost certainly use one of the above entropy libraries, and
    avoid manually managing the generator seeding.

    Similarly, although it is possible to swap the default generator and gain
    control over the random stream, this is also intended for specialized
    applications such as testing or similar scenarios where the RNG needs to be
    fully deterministic (RFC 6979, deterministic usage of DSA), or as a
    component of deterministic algorithms which internally rely on pseudorandom
    streams.

    In the general case, users should not maintain their local instances of
    {{!g}g}. All of the generators in a process have to compete for entropy, and
    it is likely that the overall result will have lower effective
    unpredictability.

    The recommended way to use these functions is either to accept an optional
    generator and pass it down, or to ignore the generator altogether, as
    illustrated in the {{!rng_examples}examples}.
*)

(** {1 Interface} *)

type g
(** A generator (PRNG) with its state. *)

exception Unseeded_generator
(** Thrown when using an uninitialized {{!g}generator}. *)

exception No_default_generator
(** Thrown when {!set_default_generator} has not been called. *)

(** Entropy sources and collection *)
module Entropy : sig

  (** Entropy sources. *)
  type source

  val sources : unit -> source list
  (** [sources ()] returns the list of available sources. *)

  val pp_source : Format.formatter -> source -> unit
  (** [pp_source ppf source] pretty-prints the entropy [source] on [ppf]. *)

  val register_source : string -> source
  (** [register_source name] registers [name] as entropy source. *)

  (** {1 Bootstrap} *)

  val whirlwind_bootstrap : int -> string
  (** [whirlwind_bootstrap id] exploits CPU-level data races which lead to
      execution-time variability. It returns 200 bytes random data prefixed
      by [id].

      See {{:http://www.ieee-security.org/TC/SP2014/papers/Not-So-RandomNumbersinVirtualizedLinuxandtheWhirlwindRNG.pdf}}
      for further details. *)

  val cpu_rng_bootstrap : (int -> string, [`Not_supported]) Result.t
  (** [cpu_rng_bootstrap id] returns 8 bytes of random data using the CPU
      RNG (rdseed). On 32bit platforms, only 4 bytes are filled.
      The [id] is used as prefix. If only rdrand is available, the return
      value is the concatenation of 512 calls to rdrand.

      @raise Failure if rdrand fails 512 times, or if rdseed fails and rdrand
      is not available.
  *)

  val bootstrap : int -> string
  (** [bootstrap id] is either [cpu_rng_bootstrap], if the CPU supports it, or
      [whirlwind_bootstrap] if not. *)

  (** {1 Timer source} *)

  val interrupt_hook : unit -> string
  (** [interrupt_hook] collects lower bytes from the cycle counter, to be
      used for entropy collection in the event loop. *)

  val timer_accumulator : g option -> unit -> unit
  (** [timer_accumulator g] is the accumulator for the timer source,
      applying {!interrupt_hook} on each call. *)

  (** {1 Periodic pulled sources} *)

  val feed_pools : g option -> source -> (unit -> (string, [ `No_random_available ]) result) -> unit
  (** [feed_pools g source f] feeds all pools of [g] using [source] by executing
      [f] for each pool. *)

  val cpu_rng : (g option -> unit -> unit, [`Not_supported]) Result.t
  (** [cpu_rng g] uses the CPU RNG (rdrand or rdseed) to feed all pools
      of [g]. It uses {!feed_pools} internally. If neither rdrand nor rdseed
      are available, [`Not_supported] is returned. *)

  val rdrand_calls : unit -> int
  (** [rdrand_calls ()] returns the number of rdrand calls. *)

  val rdrand_failures : unit -> int
  (** [rdrand_failures ()] returns the number of rdrand failures. *)

  val rdseed_calls : unit -> int
  (** [rdseed_calls ()] returns the number of rdseed calls. *)

  val rdseed_failures : unit -> int
  (** [rdseed_failures ()] returns the number of rdseed failures. *)

  (**/**)
  val id : source -> int
  (** [id source] is the identifier used for [source]. *)

  val header : int -> string -> string
  (** [header id data] constructs a unique header with [id], length of [data],
      and [data]. *)
  (**/**)
end

(** A single PRNG algorithm. *)
module type Generator = sig

  type g
  (** State type for this generator. *)

  val block : int
  (** Internally, this generator's {{!generate}generate} always produces
      [k * block] bytes. *)

  val create : ?time:(unit -> int64) -> unit -> g
  (** Create a new, unseeded {{!g}g}. *)

  val generate_into : g:g -> bytes -> off:int -> int -> unit
  [@@alert unsafe "Does not do bounds checks. Use Mirage_crypto_rng.generate_into instead."]
  (** [generate_into ~g buf ~off n] produces [n] uniformly distributed random
      bytes into [buf] at offset [off], updating the state of [g].

      Assumes that [buf] is at least [off + n] bytes long. Also assumes that
      [off] and [n] are positive integers. Caution: do not use in your
      application, use [Mirage_crypto_rng.generate_into] instead.
  *)

  val reseed : g:g -> string -> unit
  (** [reseed ~g bytes] directly updates [g]. Its new state depends both on
      [bytes] and the previous state.

      A generator is seded after a single application of [reseed]. *)

  val accumulate : g:g -> Entropy.source -> [`Acc of string -> unit]
  (** [accumulate ~g] is a closure suitable for incrementally feeding
      small amounts of environmentally sourced entropy into [g].

      Its operation should be fast enough for repeated calling from e.g.
      event loops. Systems with several distinct, stable entropy sources
      should use stable [source] to distinguish their sources. *)

  val seeded : g:g -> bool
  (** [seeded ~g] is [true] iff operations won't throw
      {{!Unseeded_generator}Unseeded_generator}. *)

  val pools : int
  (** [pools] is the amount of pools if any. *)
end

type 'a generator = (module Generator with type g = 'a)

(** Ready-to-use RNG algorithms. *)

(** {b Fortuna}, a CSPRNG {{: https://www.schneier.com/fortuna.html} proposed}
    by Schneier. *)
module Fortuna : Generator

(** {b HMAC_DRBG}: A NIST-specified RNG based on HMAC construction over the
    provided hash. *)
module Hmac_drbg (H : Digestif.S) : Generator

val create : ?g:'a -> ?seed:string -> ?strict:bool ->
  ?time:(unit -> int64) -> 'a generator -> g
(** [create ~g ~seed ~strict ~time module] uses a module conforming to the
    {{!Generator}Generator} signature to instantiate the generic generator
    {{!g}g}.

    [g] is the state to use, otherwise a fresh one is created.

    [seed] can be provided to immediately reseed the generator with.

    [strict] puts the generator into a more standards-conformant, but slighty
    slower mode. Useful if the outputs need to match published test-vectors.

    [time] is used to limit the amount of reseedings. Fortuna uses at most once
    every second. *)

val default_generator : unit -> g
(** [default_generator ()] is the default generator. Functions in this module
    use this generator when not explicitly supplied one.

    @raise No_default_generator if {!set_default_generator} has not been called. *)

val set_default_generator : g -> unit
(** [set_default_generator g] sets the default generator to [g]. This function
    must be called once. *)

(**/**)
(* This function is only used by eio to set the default generator to None when
   the entropy harvesting tasks are finished. *)
val unset_default_generator : unit -> unit
(** [unset_default_generator ()] sets the default generator to [None]. *)
(**/**)

val generate_into : ?g:g -> bytes -> ?off:int -> int -> unit
(** [generate_into ~g buf ~off len] invokes
    {{!Generator.generate_into}generate_into} on [g] or
    {{!generator}default generator}. The random data is put into [buf] starting
    at [off] (defaults to 0) with [len] bytes.

    @raise Invalid_argument if buffer is too small (it must be: [Bytes.length
      buf - off >= n]) or [off] or [n] are negative.
*)

val generate : ?g:g -> int -> string
(** Invoke {!generate_into} on [g] or {{!generator}default generator} and a
    freshly allocated string. *)

val block : g option -> int
(** {{!Generator.block}Block} size of [g] or
    {{!generator}default generator}. *)

(**/**)

(* The following functions expose the seeding interface. They are meant to
 * connect the RNG with entropy-providing libraries and subject to change.
 * Client applications should not use them directly. *)

val reseed     : ?g:g -> string -> unit
val accumulate : g option -> Entropy.source -> [`Acc of string -> unit]
val seeded     : g option -> bool
val pools      : g option -> int
val strict : g option -> bool
(**/**)


(** {1:rng_examples Examples}

    Generating a random 13-byte string:
{[let cs = Rng.generate 13]}

    Generating a list of string, passing down an optional {{!g}generator}:
{[let rec f1 ?g ~n i =
  if i < 1 then [] else Rng.generate ?g n :: f1 ?g ~n (i - 1)]}

    Generating a [Z.t] smaller than [10]:
{[let f2 ?g () = Mirage_crypto_pk.Z_extra.gen ?g Z.(~$10)]}

    Creating a local Fortuna instance and using it as a key-derivation function:
{[let f3 secret =
  let g = Rng.(create ~seed:secret (module Generators.Fortuna)) in
  Rng.generate ~g 32]}
*)
