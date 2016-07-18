#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
    Pkg.meta_file ~install:false "pkg/META.xen";
  ]

let opams =
  let install = false in
  [
    Pkg.opam_file "mirage-entropy-xen.opam" ~install;
  ]

let () =
  Pkg.describe ~metas ~opams "mirage-entropy-xen" @@ fun c ->
  match Conf.pkg_name c with
  | "mirage-entropy-xen" ->
    Ok [ Pkg.lib "pkg/META.xen" ~dst:"META";
         Pkg.clib "lib/libmirage-entropy-xen_stubs.clib";
         Pkg.mllib "lib/mirage-entropy-xen.mllib"; ]
  | other ->
    R.error_msgf "unknown package name: %s" other
