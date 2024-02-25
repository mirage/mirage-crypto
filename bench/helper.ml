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

let burn_period = 3.0

let sizes = [16; 64; 256; 1024; 8192]
(* let sizes = [16] *)

let burn f data n =
  let (t1, i1) =
    let rec loop it =
      let t = Time.time ~n:it f data in
      if t > 0.2 then (t, it) else loop (it * 10) in
    loop 10 in
  let iters = int_of_float (float i1 *. burn_period /. t1) in
  let time  = Time.time ~n:iters f data in
  (iters, time, float (n * iters) /. time)

let mb = 1024. *. 1024.

let through f gen =
  sizes |> List.map @@ fun size ->
    Gc.full_major () ;
    let data = gen size in
    let (iters, time, bw) = burn f data size in
    size, bw, iters, time

let through_str f =
  through f (fun s -> Cstruct.to_string (Mirage_crypto_rng.generate s))

let through_cs f =
  through f Mirage_crypto_rng.generate

let print_hdr title =
  Printf.printf "\n* [%s]\n%!" title

let print_result =
  List.iter (fun (size, bw, iters, time) ->
      Printf.printf "    % 5d:  %04f MB/s  (%d iters in %.03f s)\n%!"
        size (bw /. mb) iters time)

let throughput_str title f =
  print_hdr title;
  print_result (through_str f)

let throughput title f =
  print_hdr title;
  print_result (through_cs f)

let count_period = 10.

let count f n =
  ignore (f n);
  let i1 = 5 in
  let t1 = Time.time ~n:i1 f n in
  let iters = int_of_float (float i1 *. count_period /. t1) in
  let time  = Time.time ~n:iters f n in
  (iters, time)

let count title f to_str args =
  Printf.printf "\n* [%s]\n%!" title ;
  args |> List.iter @@ fun arg ->
  Gc.full_major () ;
  let iters, time = count f arg in
  Printf.printf "    %s:  %.03f ops per second (%d iters in %.03f)\n%!"
    (to_str arg) (float iters /. time) iters time
