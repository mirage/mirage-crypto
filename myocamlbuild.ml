open Ocamlbuild_plugin
open Ocb_stubblr

let intrinsics () =
  match machine () with
  | `x86 | `x86_64 ->
      flag ["compile"; "c"] (S [A "-ccopt"; A "-mrdrnd -mrdseed"])
  | _ -> ()

let () = dispatch Ocb_stubblr.(init & after_rules intrinsics)
