open Lwt

module E = Entropy

type t = { e : E.t ; token : E.token ; g : Mirage_crypto_rng.g }

let attach e g =
  let `Acc acc = Mirage_crypto_rng.accumulate (Some g) in
  E.add_handler e acc

let active = ref None
and mx     = Lwt_mutex.create ()

let initialize () =
  Mirage_crypto_rng.generator := Mirage_crypto_rng.(create (module Fortuna));
  Lwt_mutex.with_lock mx @@ fun () ->
    let g     = !Mirage_crypto_rng.generator in
    let reg e = attach e g >|= fun token -> active := Some { e ; token ; g } in
    match !active with
    | Some t when t.g == g -> return_unit
    | Some t               -> E.remove_handler t.e t.token ; reg t.e
    | None                 -> E.connect () >>= reg

let sources () =
  Mirage_crypto.Uncommon.Option.map ~f:(fun { e; _ } -> E.sources e) !active
