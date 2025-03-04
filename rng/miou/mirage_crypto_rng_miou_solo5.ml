let src = Logs.Src.create "mirage-crypto-rng.miou-solo5"

module Log = (val Logs.src_log src : Logs.LOG)

open Mirage_crypto_rng

let periodic fn delta =
  let rec one () =
    let () = fn () in
    let () = Miou_solo5.sleep delta in
    one () in
  Miou.async one

let rdrand delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> Miou.async (Fun.const ())
  | Ok cpu_rng ->
      periodic (cpu_rng None) delta

let running = Atomic.make false

let default_generator_already_set =
  "Mirage_crypto_rng.default_generator has already \
   been set (but not via Mirage_crypto_rng_miou_solo5). Please check \
   that this is intentional"

let miou_generator_already_launched =
  "Mirage_crypto_rng_miou_solo5.initialize has already been launched \
   and a task is already seeding the RNG."

type rng = Miou.Hook.t * unit Miou.t

let rec compare_and_set ?(backoff= Miou_backoff.default) t a b =
  if Atomic.compare_and_set t a b = false
  then compare_and_set ~backoff:(Miou_backoff.once backoff) t a b

let[@inline] now () = Int64.of_int (Miou_solo5.clock_monotonic ())
let _1s = 1_000_000_000

let initialize (type a) ?g ?(sleep= _1s) (rng : a generator) =
  if Atomic.compare_and_set running false true
  then begin
    let seed =
      let init = Entropy.[ bootstrap; bootstrap; whirlwind_bootstrap; bootstrap; ] in
      List.mapi (fun i fn -> fn i) init |> String.concat "" in
    let () =
      try let _ = default_generator () in
          Logs.warn (fun m -> m "%s" default_generator_already_set)
      with No_default_generator -> () in
    let rng = create ?g ~seed ~time:now rng in
    set_default_generator rng;
    let hook = Miou.Hook.add (Entropy.timer_accumulator None) in
    hook, rdrand sleep
  end else invalid_arg miou_generator_already_launched

let kill (hook, prm) =
  Miou.Hook.remove hook;
  Miou.cancel prm;
  compare_and_set running true false;
  unset_default_generator ()
