open Lwt.Infix

let time ~tag f x =
  let t1 = Unix.gettimeofday () in
  f x >|= fun res ->
    let t2 = Unix.gettimeofday () in
    Printf.printf "[time] %s: %f s\n%!" tag (t2 -. t1);
    res

let measure tag =
  let rec spin = function
    | 0 -> Lwt.return_unit
    | n -> Lwt_main.yield () >>= fun _ -> spin (pred n)
  and n = 1_000_000 in
  time ~tag:(Format.sprintf "%d spins: %s" n tag) spin n

let () =
  let main =
    measure "baseline" >>= fun _ ->
    Entropy.connect () >>= fun t ->
    measure "no handler" >>= fun _ ->
    Entropy.add_handler t (fun ~source:_ _ -> ()) >>= fun _ ->
    measure "harvesting" in
  Mirage_OS.OS.Main.run main
