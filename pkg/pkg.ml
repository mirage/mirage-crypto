#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"
open Topkg
open Ocb_stubblr_topkg

let opams = [
  Pkg.opam_file "opam" ~lint_deps_excluding:
    (Some ["mirage-xen"; "mirage-solo5"; "ocaml-freestanding"])
]

let default = false
let mirage_solo5 = Conf.with_pkg ~default "mirage-solo5"
let ocaml_freestanding = Conf.with_pkg ~default "ocaml-freestanding"
let mirage_xen = Conf.with_pkg ~default "mirage-xen"
let bench = Conf.(key "benchmark" bool ~absent:false)

let () =
  Pkg.describe ~build:(Pkg.build ~cmd ()) ~opams "mirage-entropy" @@ fun c ->
    let xen = Conf.value c mirage_xen
    and fs = Conf.(value c mirage_solo5 && value c ocaml_freestanding)
    and bench = Conf.value c bench in
    Ok ([ Pkg.mllib "lib/mirage-entropy.mllib" ;
          Pkg.test "test/test";
          Pkg.test ~cond:bench "test/test-speed";
          Pkg.clib "lib/libmirage-entropy_stubs.clib" ] @
          mirage ~xen ~fs "lib/libmirage-entropy_stubs.clib" )
