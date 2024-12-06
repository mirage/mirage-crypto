open Mirage_crypto

module Time = struct

  let time ~n f a =
    let t1 = Sys.time () in
    for _ = 1 to n do ignore (f a) done ;
    let t2 = Sys.time () in
    (t2 -. t1)

  let warmup () =
    let x = ref 0 in
    let rec go start =
      if Sys.time () -. start < 1. then begin
        for i = 0 to 10000 do x := !x + i done ;
        go start
      end in
    go (Sys.time ())

end

let burn_period = 2.0

let sizes = [16; 64; 256; 1024; 8192]
(* let sizes = [16] *)

let burn f n =
  let buf = Mirage_crypto_rng.generate n in
  let (t1, i1) =
    let rec loop it =
      let t = Time.time ~n:it f buf in
      if t > 0.2 then (t, it) else loop (it * 10) in
    loop 10 in
  let iters = int_of_float (float i1 *. burn_period /. t1) in
  let time  = Time.time ~n:iters f buf in
  (iters, time, float (n * iters) /. time)

let mb = 1024. *. 1024.

let throughput title f =
  Printf.printf "\n* [%s]\n%!" title ;
  sizes |> List.iter @@ fun size ->
    Gc.full_major () ;
    let (iters, time, bw) = burn f size in
    Printf.printf "    % 5d:  %04f MB/s  (%d iters in %.03f s)\n%!"
      size (bw /. mb) iters time

let bm name f = (name, fun () -> f name)

let benchmarks = [
  bm "pfortuna" (fun name ->
    let open Mirage_crypto_rng_miou_unix.Pfortuna in
    Miou_unix.run ~domains:2 @@ fun () ->
    let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
    let g = create () in
    reseed ~g "abcd" ;
    throughput name (fun buf ->
        let buf = Bytes.unsafe_of_string buf in
        generate_into ~g buf ~off:0 (Bytes.length buf));
    Mirage_crypto_rng_miou_unix.kill rng) ;
]

let help () =
  Printf.printf "available benchmarks:\n  ";
  List.iter (fun (n, _) -> Printf.printf "%s  " n) benchmarks ;
  Printf.printf "\n%!"

let runv fs =
  Format.printf "accel: %a\n%!"
    (fun ppf -> List.iter @@ fun x ->
      Format.fprintf ppf "%s " @@
        match x with `XOR -> "XOR" | `AES -> "AES" | `GHASH -> "GHASH")
    accelerated;
  Time.warmup () ;
  List.iter (fun f -> f ()) fs


let () =
  let seed = "abcd" in
  let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
  Mirage_crypto_rng.set_default_generator g;
  match Array.to_list Sys.argv with
  | _::(_::_ as args) -> begin
      try
        let fs =
          args |> List.map @@ fun n ->
            snd (benchmarks |> List.find @@ fun (n1, _) -> n = n1) in
        runv fs
      with Not_found -> help ()
    end
  | _ -> help ()
