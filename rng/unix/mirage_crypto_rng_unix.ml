open Mirage_crypto_rng

let src = Logs.Src.create "mirage-crypto-rng.unix" ~doc:"Mirage crypto RNG Unix"
module Log = (val Logs.src_log src : Logs.LOG)

external getrandom_buf : bytes -> int -> unit = "mc_getrandom" [@@noalloc]

let getrandom size =
  let buf = Bytes.create size in
  getrandom_buf buf size;
  Bytes.unsafe_to_string buf

let getrandom_init i =
  let data = getrandom 128 in
  Entropy.header i data

let running = Atomic.make false

let initialize (type a) ?g (rng : a generator) =
  if Atomic.get running then
    Log.debug
      (fun m -> m "Mirage_crypto_rng_unix.initialize has already been called, \
                   ignoring this call.")
  else begin
    (try
       let _ = default_generator () in
       Log.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                             been set, check that this call is intentional");
     with
       No_default_generator -> ());
    Atomic.set running true ;
    let seed =
      let init =
        Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ]
      in
      List.mapi (fun i f -> f i) init |> String.concat ""
    in
    let _ = Entropy.register_source "getrandom" in
    set_default_generator (create ?g ~seed rng)
  end
