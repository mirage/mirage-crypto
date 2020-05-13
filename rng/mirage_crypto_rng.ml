type bits = int

exception Unseeded_generator = Boot.Unseeded_generator

exception No_default_generator

exception Default_generator_already_set

let setup_rng =
  "\nTo initialize the RNG with a default generator, and set up entropy \
   collection and periodic reseeding as a background task, do the \
   following:\
   \n  If you are using MirageOS, use the random device in config.ml: \
   `let main = Mirage.foreign \"Unikernel.Main\" (random @-> job)`, \
   and `let () = register \"my_unikernel\" [main $ default_random]`. \
   \n  If you are using Lwt, execute \
   `Mirage_crypto_rng_lwt.initialize ()` at the beginning of \
   your event loop (`Lwt_main.run`) execution. \
   \n  If you're using neither MirageOS nor lwt, there is no periodic \
   reseeding. For an initial seed from getrandom(), execute \
   `Mirage_crypto_rng_unix.initialize ()`. You can use \
   `Mirage_crypto_rng.accumulate` and `Mirage_crypto_rng.reseed` to \
   reseed the RNG manually."

let () = Printexc.register_printer (function
    | Unseeded_generator ->
      Some ("The RNG has not been seeded." ^ setup_rng)
    | No_default_generator ->
      Some ("The default generator is not yet initialized. " ^ setup_rng)
    | Default_generator_already_set ->
      Some "The default generator in Mirage_crypto_rng has already been set, \
            and can only be set once in the program lifetime. The reason for \
            this is to avoid potential issues with entropy collection and \
            distribution."
    | _ -> None)

module type Generator = sig
  type g
  val block      : int
  val create     : unit -> g
  val generate   : g:g -> int -> Cstruct.t
  val reseed     : g:g -> Cstruct.t -> unit
  val accumulate : g:g -> source:int -> [`Acc of Cstruct.t -> unit]
  val seeded     : g:g -> bool
end

type 'a generator = (module Generator with type g = 'a)
type g = Generator : ('a * bool * 'a generator) -> g

module Fortuna = Fortuna

module Hmac_drbg = Hmac_drbg.Make

let create (type a) ?g ?seed ?(strict=false) (m : a generator) =
  let module M = (val m) in
  let g = Option.value g ~default:(M.create ()) in
  Option.iter (M.reseed ~g) seed;
  Generator (g, strict, m)

let _default_generator = ref None

let set_default_generator g =
  match !_default_generator with
  | None -> _default_generator := Some g
  | Some _ -> raise Default_generator_already_set

let default_generator () =
  match !_default_generator with
  | None -> raise No_default_generator
  | Some g -> g

let get = function Some g -> g | None -> default_generator ()

let generate ?(g = default_generator ()) n =
  let Generator (g, _, m) = g in let module M = (val m) in M.generate ~g n

let reseed ?(g = default_generator ()) cs =
  let Generator (g, _, m) = g in let module M = (val m) in M.reseed ~g cs

let accumulate g ~source =
  let Generator (g, _, m) = get g in
  let module M = (val m) in
  M.accumulate ~g ~source

let seeded g =
  let Generator (g, _, m) = get g in let module M = (val m) in M.seeded ~g

let block g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.block

let strict g =
  let Generator (_, s, _) = get g in s
