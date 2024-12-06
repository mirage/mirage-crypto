
type g = In_channel.t * Mutex.t

let block = 2048

let create ?time:_ () =
  let ic = In_channel.open_bin "/dev/urandom"
  and mutex = Mutex.create ()
  in
  at_exit (fun () -> In_channel.close ic);
  (ic, mutex)

let generate_into ~g:(ic, m) buf ~off len =
  let finally () = Mutex.unlock m in
  Mutex.lock m;
  Fun.protect ~finally (fun () ->
      match In_channel.really_input ic buf off len with
      | None -> failwith "couldn't read enough bytes from /dev/urandom"
      | Some () -> ())

let reseed ~g:_ _data = ()

let accumulate ~g:_ _source =
  `Acc (fun _data -> ())

let seeded ~g:_ = true

let pools = 0
