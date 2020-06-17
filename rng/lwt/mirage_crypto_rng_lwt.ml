open Mirage_crypto_rng

let periodic f delta =
  let open Lwt.Infix in
  Lwt.async (fun () ->
      let rec one () =
        f (); Lwt_unix.sleep (Duration.to_f delta) >>= one
      in
      one ())

let getrandom_task delta =
  let task () =
    let per_pool = 8 in
    let size = per_pool * pools None in
    let random = Mirage_crypto_rng_unix.getrandom size in
    let idx = ref 0 in
    let f () =
      incr idx;
      Cstruct.sub random (per_pool * (pred !idx)) per_pool
    in
    Entropy.feed_pools None `Getrandom f
  in
  periodic task delta

let rdrand_task delta =
  let task () = Entropy.cpu_rng None in
  periodic task delta

let running = ref false

let getrandom_init _ =
  let data = Mirage_crypto_rng_unix.getrandom 128 in
  Entropy.header `Getrandom data

let initialize ?(sleep = Duration.of_sec 1) () =
  if !running then
    Logs.info
      (fun m -> m "Mirage_crypto_rng_lwt.initialize has already been called, \
                   ignoring this call.")
  else begin
    (try
       let _ = default_generator () in
       Logs.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                              been set (but not via \
                              Mirage_crypto_rng_lwt.initialize). Please check \
                              that this is intentional");
     with
       No_default_generator -> ());
    running := true;
    let seed =
      let init =
        Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ]
      in
      List.mapi (fun i f -> f i) init |> Cstruct.concat
    in
    Entropy.add_source `Getrandom;
    let rng = create ~seed ~time:Mtime_clock.elapsed_ns (module Fortuna) in
    set_default_generator rng;
    rdrand_task sleep;
    getrandom_task (Int64.mul sleep 10L);
    let _ =
      Lwt_main.Enter_iter_hooks.add_first (Entropy.timer_accumulator None)
    in
    ()
  end
