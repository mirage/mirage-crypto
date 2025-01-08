type source = int * string

exception Unseeded_generator

exception No_default_generator

let setup_rng =
  "\nPlease setup your default random number generator. On Unix, the best \
   path is to call [Mirage_crypto_rng_unix.use_default ()].\
   \nBut you can use Fortuna (or any other RNG) and setup the seeding \
   (done by default in MirageOS): \
   \n\
   \nTo initialize the RNG with a default generator, and set up entropy \
   collection and periodic reseeding as a background task, do the \
   following:\
   \n  If you are using MirageOS, use the random device in config.ml: \
   `let main = Mirage.main \"Unikernel.Main\" (random @-> job)`, \
   and `let () = register \"my_unikernel\" [main $ default_random]`. \
   \n  If you are using miou, execute \
   `Mirage_crypto_rng_miou_unix.initialize (module Mirage_crypto_rng.Fortuna)` \
   at startup."

let () = Printexc.register_printer (function
    | Unseeded_generator ->
      Some ("The RNG has not been seeded." ^ setup_rng)
    | No_default_generator ->
      Some ("The default generator is not yet initialized. " ^ setup_rng)
    | _ -> None)

module type Generator = sig
  type g
  val block : int
  val create : ?time:(unit -> int64) -> unit -> g
  val generate_into : g:g -> bytes -> off:int -> int -> unit
  [@@alert unsafe "Does not do bounds checks. Use Mirage_crypto_rng.generate_into instead."]
  val reseed : g:g -> string -> unit
  val accumulate : g:g -> source -> [`Acc of string -> unit]
  val seeded : g:g -> bool
  val pools : int
end

type 'a generator = (module Generator with type g = 'a)
type g = Generator : ('a * bool * 'a generator) -> g

let create (type a) ?g ?seed ?(strict=false) ?time (m : a generator) =
  let module M = (val m) in
  let g = Option.value g ~default:(M.create ?time ()) in
  Option.iter (M.reseed ~g) seed;
  Generator (g, strict, m)

let _default_generator = Atomic.make None

let set_default_generator g = Atomic.set _default_generator (Some g)

let unset_default_generator () = Atomic.set _default_generator None

let default_generator () =
  match Atomic.get _default_generator with
  | None -> raise No_default_generator
  | Some g -> g

let get = function Some g -> g | None -> default_generator ()

let generate_into ?(g = default_generator ()) b ?(off = 0) n =
  let Generator (g, _, m) = g in
  let module M = (val m) in
  if off < 0 || n < 0 then
    invalid_arg ("negative offset " ^ string_of_int off ^ " or length " ^
                 string_of_int n);
  if Bytes.length b - off < n then
    invalid_arg "buffer too short";
  begin[@alert "-unsafe"]
    M.generate_into ~g b ~off n
  end

let generate ?g n =
  let data = Bytes.create n in
  generate_into ?g data ~off:0 n;
  Bytes.unsafe_to_string data

let reseed ?(g = default_generator ()) cs =
  let Generator (g, _, m) = g in let module M = (val m) in M.reseed ~g cs

let accumulate g source =
  let Generator (g, _, m) = get g in
  let module M = (val m) in
  M.accumulate ~g source

let seeded g =
  let Generator (g, _, m) = get g in let module M = (val m) in M.seeded ~g

let block g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.block

let pools g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.pools

let strict g =
  let Generator (_, s, _) = get g in s
