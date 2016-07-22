#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opams = [
  Pkg.opam_file "opam" ~lint_deps_excluding:
    (Some ["mirage-xen"; "mirage-solo5"; "ocaml-freestanding"])
]

let mirage_solo5 = Conf.with_pkg "mirage-solo5"
let ocaml_freestanding = Conf.with_pkg "ocaml-freestanding"
let mirage_xen = Conf.with_pkg "mirage-xen"

let xen c = Conf.value c mirage_xen
let solo5 c = Conf.(value c mirage_solo5 && value c ocaml_freestanding)

let () =
  Pkg.describe ~opams "mirage-entropy" @@ fun c ->
    Ok [ Pkg.mllib "lib/mirage-entropy.mllib" ;
         Pkg.clib "lib/libmirage-entropy_stubs.clib" ;
         Pkg.clib ~cond:(xen c) "xen/libmirage-entropy-xen_stubs.clib" ;
         Pkg.clib ~cond:(solo5 c) "solo5/libmirage-entropy-freestanding_stubs.clib"
    ]
