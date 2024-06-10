(* Pfortuna is a re-implementation of Fortuna with a mutex. The goal of this
   module is to provide a global and domain-safe RNG. The implementation use
   [Miou.Mutex] instead of [Mutex] - [Pfortuna] is only available as part of
   the [mirage-crypto-rng-miou-unix] package. Thus, in the context of Miou,
   [Pfortuna] can be used and recommended in place of [Fortuna], so that the
   user can generate random numbers in parallel in several domains.

   {[
     let () = Miou_unix.run @@ fun () ->
       let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
       ...
       Mirage_crypto_rng_miou_unix.kill rng
   ]}

   NOTE: when modifying this file, please also check whether rng/fortuna.ml
   needs to be updated. *)

open Mirage_crypto
open Mirage_crypto.Uncommon

module SHAd256 = struct
  open Digestif
  type ctx = SHA256.ctx
  let empty     = SHA256.empty
  let get t     = SHA256.(get t |> to_raw_string |> digest_string |> to_raw_string)
  let digesti i = SHA256.(digesti_string i |> to_raw_string |> digest_string |> to_raw_string)
  let feedi     = SHA256.feedi_string
end

let block = 16

(* the minimal amount of bytes in a pool to trigger a reseed *)
let min_pool_size = 64
(* the minimal duration between two reseeds *)
let min_time_duration = 1_000_000_000L
(* number of pools *)
let pools = 32

type t =
  { ctr          : AES.CTR.ctr
  ; secret       : string
  ; key          : AES.CTR.key
  ; pools        : SHAd256.ctx array
  ; pool0_size   : int
  ; reseed_count : int
  ; last_reseed  : int64
  ; time         : (unit -> int64) option
  }

type g = Miou.Mutex.t * t ref

let update (m, g) fn = Miou.Mutex.protect m @@ fun () -> g := fn !g
let get (m, g) fn = Miou.Mutex.protect m @@ fun () -> fn !g

let create ?time () =
  let secret = String.make 32 '\000' in
  let m = Miou.Mutex.create () in
  let t =
    { ctr= (0L, 0L); secret; key= AES.CTR.of_secret secret
    ; pools= Array.make pools SHAd256.empty
    ; pool0_size= 0
    ; reseed_count= 0
    ; last_reseed= 0L
    ; time } in
  (m, { contents= t })

let seeded ~t =
  let lo, hi = t.ctr in
  not (Int64.equal lo 0L && Int64.equal hi 0L)

let set_key ~t secret =
  { t with secret; key= AES.CTR.of_secret secret }

let reseedi ~t iter =
  let t = set_key ~t (SHAd256.digesti (fun fn -> fn t.secret; iter fn)) in
  { t with ctr= AES.CTR.add_ctr t.ctr 1L }

let iter1 a f = f a
let reseed ~t cs = reseedi ~t (iter1 cs)

let generate_rekey ~t buf ~off len =
  let b = len // block* 2 in
  let n = b * block in
  let r = AES.CTR.stream ~key:t.key ~ctr:t.ctr n in
  Bytes.unsafe_blit_string r 0 buf off len;
  let r2 = String.sub r (n - 32) 32 in
  let t = set_key ~t r2 in
  { t with ctr= AES.CTR.add_ctr t.ctr (Int64.of_int b) }

let add_pool_entropy t =
  if t.pool0_size > min_pool_size then
    let should_reseed, now = match t.time with
      | None -> true, 0L
      | Some fn ->
        let now = fn () in
        Int64.(sub now t.last_reseed > min_time_duration), now in
  if should_reseed then begin
    let t = { t with reseed_count= t.reseed_count + 1
                   ; last_reseed= now
                   ; pool0_size= 0 } in
    reseedi ~t @@ fun add ->
    for i = 0 to pools - 1 do
      if t.reseed_count land ((1 lsl i) - 1) = 0
      then (SHAd256.get t.pools.(i) |> add; t.pools.(i) <- SHAd256.empty)
    done
  end else t else t

let generate_into ~t buf ~off len =
  let t = add_pool_entropy t in
  if not (seeded ~t) then raise Mirage_crypto_rng.Unseeded_generator;
  let rec chunk t off = function
    | i when i <= 0 -> t
    | n ->
      let n' = imin n 0x10000 in
      let t = generate_rekey ~t buf ~off n' in
      chunk t (off + n') (n - n') in
  chunk t off len

let add ~t source ~pool data =
  let buf = Bytes.create 2
  and pool = pool land (pools - 1)
  and source = Mirage_crypto_rng.Entropy.id source land 0xff in
  Bytes.set_uint8 buf 0 source;
  Bytes.set_uint8 buf 1 (String.length data);
  t.pools.(pool) <- SHAd256.feedi t.pools.(pool) (iter2 (Bytes.unsafe_to_string buf) data);
  if pool = 0 then { t with pool0_size= t.pool0_size + String.length data } else t

let accumulate ~g source =
  let pool = ref 0 in
  `Acc (fun buf ->
     update g @@ fun t ->
     let t = add ~t source ~pool:!pool buf in
     incr pool; t)

let reseed ~g cs = update g @@ fun t -> reseed ~t cs
let generate_into ~g buf ~off len = update g @@ fun t -> generate_into ~t buf ~off len
let seeded ~g = get g @@ fun t -> seeded ~t
