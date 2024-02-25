
open Helper

let secret_hex = "4c6db7cf935bcf84026178d40c956af09d8e363203490d2c41625acb68b931a4"
let secret_cs = Cstruct.of_hex secret_hex
let secret_str = Cstruct.to_string secret_cs

let share_hex = "ca19193cf5c0b38c61aa01c172b2e93d16f750d0846277ad322de5e4fb332429"
let share_cs = Cstruct.of_hex share_hex
let share_str = Cstruct.to_string share_cs

let res_hex = "075137d4b326c438c9634ecaffb8f7f60b51f3a6fe2bcb5a3b21db1101f70b07"
let res_cs = Cstruct.of_hex res_hex
let res_str = Cstruct.to_string res_cs

let ecdh_shares = [
  ("mirage-crypto", `Mirage_crypto (Mirage_crypto_ec.X25519.secret_of_cs secret_cs |> Result.get_ok |> fst, share_cs));
  ("callipyge", `Callipyge Callipyge.(secret_key_of_string secret_str, public_key_of_string share_str));
  ("rfc7748", `Rfc7748 Rfc7748.X25519.(private_key_of_string secret_hex, public_key_of_string share_hex));
  ("hacl_x25519", `Hacl_x25519 Hacl_x25519.(gen_key ~rng:(fun _ -> secret_cs) |> fst, share_cs));
  ("openssl", `Openssl)
]

let bm name f = (name, fun () -> f name)

let openssl_speed args =
  let cmd = Bos.Cmd.(v "openssl" % "speed" % "-mr" %% of_list args) in
  print_endline ("  benchmarking " ^ Bos.Cmd.to_string cmd);
  match Bos.(OS.Cmd.(run_out ~err:err_null cmd |> out_lines |> success)) with
  | Error `Msg m -> invalid_arg m
  | Ok lines ->
    (* we're looking for +F?:?:?arg:V:? *)
    let ops =
      List.fold_left (fun acc line ->
          match acc with
          | None when String.(equal (sub line 0 2) "+F") ->
            (match String.split_on_char ':' line with
             | _f :: _ :: _algo :: v :: _rest ->
               Some (float_of_string v)
             | _ -> invalid_arg ("unexpected line starting with +F: " ^ line))
          | _ -> acc)
        None lines
    in
    "openssl", [ 1, Option.get ops ]

let c name f n =
  print_endline ("  benchmarking " ^ name ^ "...");
  Gc.full_major () ;
  let iters, time = count_it f n in
  name, [ 1, float iters /. time ]

let benchmarks = [
  ("ecdh-share", fun () ->
      print_endline "ecdh-share";
      let res =
        List.map (fun (name, x) -> match x with
            | `Mirage_crypto (sec, share) ->
              c name (fun share ->
                  let out = Mirage_crypto_ec.X25519.key_exchange sec share |> Result.get_ok in
                  ignore out;
                  (* assert (Cstruct.equal out res_cs) *))
                share
            | `Callipyge (secret, public) ->
              c name (fun public ->
                  let out = Callipyge.shared ~secret ~public in
                  ignore out;
                  (* assert (String.equal (Callipyge.string_of_key out) res_str) *))
                public
            | `Rfc7748 (sec, share) ->
              c name (fun share ->
                  let out = Rfc7748.X25519.scale sec share in
                  ignore out;
                  (* assert (String.equal (Rfc7748.X25519.string_of_public_key out) res_hex) *))
                share
            | `Hacl_x25519 (sec, share) ->
              c name (fun share ->
                  let out = Hacl_x25519.key_exchange sec share |> Result.get_ok in
                  ignore out;
                  (* assert (Cstruct.equal out res_cs) *))
                share
             | `Openssl -> openssl_speed [ "ecdhx25519" ])
          ecdh_shares
      in
      let footer = "All numbers in operations per second" in
      print_result_table ~first:"" ~footer "X25519" res)

]

let help () =
  Printf.printf "available benchmarks:\n  ";
  List.iter (fun (n, _) -> Printf.printf "%s  " n) benchmarks ;
  Printf.printf "\n%!"

let runv fs =
  Time.warmup () ;
  List.iter (fun f -> f ()) fs

let () =
  let seed = Cstruct.of_string "abcd" in
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
