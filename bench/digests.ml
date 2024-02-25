open Helper

let print_result_table alg rs =
  if rs = [] then
    print_endline "rs is empty"
  else
    let first = "size", List.map fst (snd (List.hd rs)) in
    let col_width = 11 in
    let space ?(extra = 0) s = String.make (col_width - String.length s - extra) ' ' in
    print_endline "";
    print_endline "------------";
    print_endline ("| " ^ alg ^ space ~extra:3 alg ^ " |");
    print_endline "------------";
    (let extra = 5 in
     if String.length (fst first) > col_width - extra then
       Printf.printf "%s " (String.sub (fst first) 0 (col_width - extra - 1))
     else
       Printf.printf "%s%s" (fst first) (space ~extra (fst first)));
    let rpad s =
      if String.length s > col_width - 1 then
        Printf.printf "%s " (String.sub s 0 (col_width - 1))
      else
        Printf.printf "%s%s " (space ~extra:1 s) s
    in
    List.iter rpad (List.map fst rs);
    Printf.printf "\n";
    List.iter (fun size ->
        Printf.printf "%5d " size;
        let vals =
          List.map (fun data ->
              Option.value ~default:0.0 (List.assoc_opt size data) /. mb)
            (List.map snd rs)
        in
        let max_bw = List.fold_left Float.max 0.0 vals in
        List.iter (fun v ->
            let st = if Float.equal max_bw v then "*" else " " in
            Printf.printf " %s%8.3f%s" st v st)
          vals;
        Printf.printf "\n";
      ) (snd first);
    Printf.printf "First column in bytes, all others in MB/s (1MB = 1024 * 1024)\n\n%!"

let openssl_speed args =
  let cmd = Bos.Cmd.(v "openssl" % "speed" % "-mr" %% of_list args) in
  print_endline ("  benchmarking " ^ Bos.Cmd.to_string cmd);
  match Bos.(OS.Cmd.(run_out ~err:err_null cmd |> out_lines |> success)) with
  | Error `Msg m -> invalid_arg m
  | Ok lines ->
    (* we're looking for +H:N0:N1:N2... and subsequent +F:?:?arg:V1:V2:V3 *)
    let sizes, bw =
      List.fold_left (fun acc line ->
          match acc with
          | None, None when String.(equal (sub line 0 3) "+H:") ->
            Some (List.map int_of_string (List.tl (String.split_on_char ':' line))), None
          | Some _ as s, None when String.(equal (sub line 0 3) "+F:") ->
            (match String.split_on_char ':' line with
             | _f :: _ :: algo :: rest -> s, Some (algo, List.map float_of_string rest)
             | _ -> invalid_arg ("unexpected line starting with +F: " ^ line))
          | _ -> acc)
        (None, None) lines
    in
    let s = Option.get sizes
    and _alg, bw = Option.get bw
    in
    "openssl", List.combine s bw

let bench name f a =
  print_endline ("  benchmarking " ^ name ^ "...");
  name, List.map (fun (s, v, _, _) -> (s, v)) (f a)

let benchmarks = [
  ("md5", fun () ->
      print_endline "MD5";
      let nc = bench "nocrypto" through_cs Nocrypto.Hash.MD5.digest in
      let mc = bench "mirage-crypto" through_cs Mirage_crypto.Hash.MD5.digest in
      let di = bench "digestif" through_str Digestif.MD5.digest_string in
      let std = bench "stdlib" through_str Digest.string in
      let ck = bench "cryptokit" through_str Cryptokit.(hash_string (Hash.md5 ())) in
      let os = openssl_speed [ "md5" ] in
      let rs = [ nc ; mc ; di ; std ; ck ; os ]
      in
      print_result_table "MD5" rs;
  );

  ("sha1", fun () ->
      print_endline "SHA1";
      let nc = bench "nocrypto" through_cs Nocrypto.Hash.SHA1.digest in
      let mc = bench "mirage-crypto" through_cs Mirage_crypto.Hash.SHA1.digest in
      let di = bench "digestif" through_str Digestif.SHA1.digest_string in
      let std = bench "ocaml-sha" through_str Sha1.string in
      let ck = bench "cryptokit" through_str Cryptokit.(hash_string (Hash.sha1 ())) in
      let os = openssl_speed [ "sha1" ] in
      let rs = [ nc ; mc ; di ; std ; ck ; os ] in
      print_result_table "SHA1" rs;
  ) ;

  ("sha256", fun () ->
      print_endline "SHA256";
      let nc = bench "nocrypto" through_cs Nocrypto.Hash.SHA256.digest in
      let mc = bench "mirage-crypto" through_cs Mirage_crypto.Hash.SHA256.digest in
      let di = bench "digestif" through_str Digestif.SHA256.digest_string in
      let std = bench "ocaml-sha" through_str Sha256.string in
      let ck = bench "cryptokit" through_str Cryptokit.(hash_string (Hash.sha256 ())) in
      let os = openssl_speed [ "sha256" ] in
      let rs = [ nc ; mc ; di ; std ; ck ; os ] in
      print_result_table "SHA256" rs;
  ) ;

  ("sha512", fun () ->
      print_endline "SHA512";
      let nc = bench "nocrypto" through_cs Nocrypto.Hash.SHA512.digest in
      let mc = bench "mirage-crypto" through_cs Mirage_crypto.Hash.SHA512.digest in
      let di = bench "digestif" through_str Digestif.SHA512.digest_string in
      let std = bench "ocaml-sha" through_str Sha512.string in
      let ck = bench "cryptokit" through_str Cryptokit.(hash_string (Hash.sha512 ())) in
      let os = openssl_speed [ "sha512" ] in
      let rs = [ nc ; mc ; di ; std ; ck ; os ] in
      print_result_table "SHA512" rs;
  ) ;
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
