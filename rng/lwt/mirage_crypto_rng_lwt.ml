open Mirage_crypto_rng

let periodic f delta =
  let open Lwt.Infix in
  Lwt.async (fun () ->
      let rec one () =
        f (); Lwt_unix.sleep (Duration.to_f delta) >>= one
      in
      one ())

let getrandom_task g delta =
  let task () =
    let per_pool = 8 in
    let size = per_pool * pools (Some g) in
    let random = Mirage_crypto_rng_unix.getrandom size in
    let idx = ref 0 in
    let f () =
      incr idx;
      Cstruct.sub random (per_pool * (pred !idx)) per_pool
    in
    Entropy.feed_pools g `Getrandom f
  in
  periodic task delta

let rdrand_task g delta =
  let task () = Entropy.cpu_rng g in
  periodic task delta

let running = ref false

let getrandom_init _ =
  let data = Mirage_crypto_rng_unix.getrandom 128 in
  Entropy.header `Getrandom data

let initialize ?(sleep = Duration.of_sec 1) () =
  if !running then
    Lwt.fail_with "entropy collection already running"
  else begin
    running := true;
    let seed =
      List.mapi (fun i f -> f i)
        Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ] |>
      Cstruct.concat
    in
    Entropy.add_source `Getrandom;
    let rng = create ~seed ~time:Mtime_clock.elapsed_ns (module Fortuna) in
    set_default_generator rng;
    rdrand_task rng sleep;
    getrandom_task rng (Int64.mul sleep 10L);
    let _ = Lwt_main.Enter_iter_hooks.add_first (Entropy.timer_accumulator rng) in
    Lwt.return_unit
  end
