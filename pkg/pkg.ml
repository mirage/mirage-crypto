#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
    Pkg.meta_file ~install:false "pkg/META.xen";
    Pkg.meta_file ~install:false "pkg/META.solo5";
  ]

let opams =
  let install = false in
  [
    Pkg.opam_file "mirage-entropy-xen.opam" ~install;
    Pkg.opam_file "mirage-entropy-solo5.opam" ~install;
  ]

let cmd c os files =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
  let flags =
    match Conf.pkg_name c with
    | "mirage-entropy-xen" -> Cmd.(v "-pkg" % "mirage-xen" % "-tag" % "use_xen_stubs")
    | "mirage-entropy-solo5" -> Cmd.(v "-pkg" % "mirage-solo5" % "-tag" % "use_solo5_stubs")
    | _ -> invalid_arg "unknown package name"
  in
  OS.Cmd.run @@
  Cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %% debug %
                    "-build-dir" % build_dir %% flags %% of_list files)

let build = Pkg.build ~cmd ()

let () =
  Pkg.describe ~build ~metas ~opams "mirage-entropy-xen" @@ fun c ->
  match Conf.pkg_name c with
  | "mirage-entropy-xen" ->
    Ok [ Pkg.lib "pkg/META.xen" ~dst:"META";
         Pkg.clib "lib/libmirage-entropy_stubs.clib";
         Pkg.mllib "lib/mirage-entropy.mllib"; ]
  | "mirage-entropy-solo5" ->
    Ok [ Pkg.lib "pkg/META.solo5" ~dst:"META";
         Pkg.clib "lib/libmirage-entropy_stubs.clib";
         Pkg.mllib "lib/mirage-entropy.mllib"; ]
  | other ->
    R.error_msgf "unknown package name: %s" other
