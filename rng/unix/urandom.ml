type g = Unix.file_descr

let block = 1

let create ?time:_ () =
  let fd =
    Unix.openfile "/dev/urandom" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0
  in
  at_exit (fun () -> Unix.close fd);
  fd

let rec generate_into ~g buf ~off len =
  if len > 0 then
    try
      let n = Unix.read g buf off len in
      if n = 0 then failwith "couldn't read enough bytes from /dev/urandom"
      else generate_into ~g buf ~off:(off + n) (len - n)
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> generate_into ~g buf ~off len

let reseed ~g:_ _data = ()

let accumulate ~g:_ _source =
  `Acc (fun _data -> ())

let seeded ~g:_ = true

let pools = 0
