open Mirage_crypto_rng

module Pfortuna = Pfortuna

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
external reraise : exn -> 'a = "%reraise"

let periodic fn delta =
  let rec one () =
    fn ();
    Miou_unix.sleep (Duration.to_f delta);
    one () in
  Effect.perform (Spawn one)

let getrandom delta source =
  let fn () =
    let per_pool = 8 in
    let size = per_pool * pools None in
    let random = Mirage_crypto_rng_unix.getrandom size in
    let idx = ref 0 in
    let fn () =
      incr idx;
      Ok (String.sub random (per_pool * (pred !idx)) per_pool)
    in
    Entropy.feed_pools None source fn in
  periodic fn delta

let getrandom_init i =
  let data = Mirage_crypto_rng_unix.getrandom 128 in
  Entropy.header i data

let rdrand delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> ()
  | Ok cpu_rng -> periodic (cpu_rng None) delta

let running = Atomic.make false

let switch fn =
  let orphans = Miou.orphans () in
  let open Effect.Deep in
  let retc = Fun.id in
  let exnc = reraise in
  let effc : type c. c Effect.t -> ((c, 'r) continuation -> 'r) option
    = function
    | Spawn fn ->
      ignore (Miou.async ~orphans fn);
      Some (fun k -> continue k ())
    | _ -> None in
  match_with fn orphans { retc; exnc; effc }

let default_generator_already_set =
  "Mirage_crypto_rng.default_generator has already \
   been set (but not via Mirage_crypto_rng_miou). Please check \
   that this is intentional"

let miou_generator_already_launched =
  "Mirage_crypto_rng_miou.initialize has already been launched \
   and a task is already seeding the RNG."

type rng = unit Miou.t

let rec compare_and_set ?(backoff= Miou_backoff.default) t a b =
  if Atomic.compare_and_set t a b = false
  then compare_and_set ~backoff:(Miou_backoff.once backoff) t a b

let rec clean_up sleep orphans = match Miou.care orphans with
  | Some None | None -> Miou_unix.sleep (Duration.to_f sleep); clean_up sleep orphans
  | Some (Some prm) -> Miou.await_exn prm; clean_up sleep orphans

let call_if_domain_available fn =
  let available = Miou.Domain.available () in
  let current = (Stdlib.Domain.self () :> int) in
  if current = 0 && available > 0
  || current <> 0 && available > 1
  then Miou.call fn
  else Miou.async fn

let initialize (type a) ?g ?(sleep= Duration.of_sec 1) (rng : a generator) =
  if Atomic.compare_and_set running false true
  then begin
    let seed =
      let init = Entropy.[ bootstrap; whirlwind_bootstrap; bootstrap; getrandom_init ] in
      List.mapi (fun i fn -> fn i) init |> String.concat "" in
    let () =
      try let _ = default_generator () in
          Logs.warn (fun m -> m "%s" default_generator_already_set)
      with No_default_generator -> () in
    let rng = create ?g ~seed ~time:Mtime_clock.elapsed_ns rng in
    set_default_generator rng;
    call_if_domain_available @@ fun () -> switch @@ fun orphans ->
    rdrand sleep;
    let source = Entropy.register_source "getrandom" in
    getrandom (Int64.mul sleep 10L) source;
    clean_up sleep orphans
  end else invalid_arg miou_generator_already_launched

let kill prm =
  Miou.cancel prm;
  compare_and_set running true false;
  unset_default_generator ()
