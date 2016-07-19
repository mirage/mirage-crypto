open Ocamlbuild_plugin
open Command

let to_opt = List.fold_left (fun acc x -> [A "-ccopt"; A x] @ acc) []

let arch = Ocamlbuild_pack.My_unix.run_and_read "uname -m"

let ccopt = match arch with
  | "amd64\n" | "x86_64\n" | "i686\n" -> to_opt ["-mrdrnd"; "-mrdseed"]
  | _ -> []

let common_ccopt = to_opt [ "-O3" ; "-std=c99" ; "-Wall" ; "-Wpedantic" ]

let pkg_config_flags pkg =
  match Ocamlbuild_pack.My_unix.run_and_read ("./postconf.sh " ^ pkg) with
  | "\n" -> []
  | x -> to_opt [String.sub x 0 (pred (String.length x))]

let () =
  dispatch begin function
    | After_rules ->
      copy_rule "copy generated source to xen directory"
        "lib/%"
        "xen/%" ;
      copy_rule "copy generated source to solo5 directory"
        "lib/%"
        "solo5/%" ;
      let flags = common_ccopt @ ccopt in
      flag ["c"; "use_xen_stubs"; "compile"] &
        S (flags @ pkg_config_flags "mirage-xen") ;
      flag ["c"; "use_solo5_stubs"; "compile"] &
        S (flags @ pkg_config_flags "ocaml-freestanding") ;
  | _ -> ()
  end
