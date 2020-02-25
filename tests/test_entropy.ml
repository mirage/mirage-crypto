open Lwt.Infix

let fpr ppf fmt = Format.fprintf ppf fmt

let pp_cs ppf cs =
  fpr ppf "@,";
  for i = 0 to Cstruct.len cs - 1 do
    fpr ppf "%02x" (Cstruct.get_uint8 cs i)
  done;
  fpr ppf "@,"

let handler ~source buf =
  Format.printf "recv: (src:%d) %a%!" source pp_cs buf

let with_entropy act =
  Mirage_crypto_entropy.connect () >>= fun t ->
  Mirage_crypto_entropy.add_handler t handler >>= fun _tok ->
  act () >>= fun res ->
  Mirage_crypto_entropy.disconnect t >|= fun () -> res

let () =
  OS.(Main.run (with_entropy (fun () ->
      Time.sleep_ns 1_000L)))
