let src = Logs.Src.create "mirage-crypto-rng.miou-solo5"

module Log = (val Logs.src_log src : Logs.LOG)

open Mirage_crypto_rng

let periodic fn delta =
  let rec one () =
    let () = fn () in
    let () = Mkernel.sleep delta in
    one () in
  Miou.async one

let rdrand delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> None
  | Ok cpu_rng ->
      Some (periodic (cpu_rng None) delta)

let running = Atomic.make false

let default_generator_already_set =
  "Mirage_crypto_rng.default_generator has already \
   been set (but not via Mirage_crypto_rng_mkernel). Please check \
   that this is intentional"

let miou_generator_already_launched =
  "Mirage_crypto_rng_mkernel.initialize has already been launched \
   and a task is already seeding the RNG."

type rng = Mkernel.Hook.t * unit Miou.t option

let rec compare_and_set ?(backoff= Miou_backoff.default) t a b =
  if Atomic.compare_and_set t a b = false
  then compare_and_set ~backoff:(Miou_backoff.once backoff) t a b

let[@inline] now () = Int64.of_int (Mkernel.clock_monotonic ())
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
    let hook = Mkernel.Hook.add (Entropy.timer_accumulator None) in
    (hook, rdrand sleep)
  end else invalid_arg miou_generator_already_launched

let kill (hook, prm) =
  Mkernel.Hook.remove hook;
  Option.iter Miou.cancel prm;
  compare_and_set running true false;
  unset_default_generator ()
