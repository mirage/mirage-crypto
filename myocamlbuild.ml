open Ocamlbuild_plugin
open Command

let to_opt = List.fold_left (fun acc x -> [A "-ccopt"; A x] @ acc) []

let arch = Ocamlbuild_pack.My_unix.run_and_read "uname -m"

let ccopt = match arch with
  | "amd64\n" | "x86_64\n" | "i686\n" -> to_opt ["-mrdrnd"; "-mrdseed"]
  | _ -> []

let common_ccopt = to_opt [ "-O3" ; "-std=c99" ; "-Wall" ; "-Wpedantic" ]

let xen_ccopt =
  let pkg = "mirage-xen" in
  let cmd =
    String.concat " "
      [ "PKG_CONFIG_PATH=`opam config var prefix`/lib/pkgconfig pkg-config --static" ;
        pkg ;
        " --cflags" ]
  in
  match Ocamlbuild_pack.My_unix.run_and_read cmd with
  | "\n" -> []
  | x -> to_opt [String.sub x 0 (pred (String.length x))]

let () =
  dispatch begin function
  | After_rules ->
    flag ["c"; "use_xen_stubs"; "compile"] & S (common_ccopt @ ccopt @ xen_ccopt) ;
  | _ -> ()
  end
