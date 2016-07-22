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
        "lib/native/%"
        "xen/native/%" ;
      copy_rule "copy generated source to solo5 directory"
        "lib/native/%"
        "solo5/native/%" ;

      flag ["link"; "ocaml"; "library"; "byte"; "dynamic_link_to_unix_stubs"] &
        S [A "-dllib"; A "-lmirage-entropy_stubs"] ;
      flag ["link"; "ocaml"; "library"; "native"; "dynamic_link_to_unix_stubs"] &
        S [A "-cclib"; A "-lmirage-entropy_stubs"] ;

      flag ["link"; "ocaml"; "static_link_to_unix_stubs"] &
        S [A "lib/libmirage-entropy_stubs.a"] ;
      dep ["link"; "ocaml"; "static_link_to_unix_stubs"] &
        ["lib/libmirage-entropy_stubs.a"] ;

      flag ["compile"; "c"; "use_c_flags"] &
        S (common_ccopt @ ccopt) ;
      flag ["compile"; "c"; "use_xen_pkgconfig_flags"] &
        S (pkg_config_flags "mirage-xen") ;
      flag ["compile"; "c"; "use_solo5_pkgconfig_flags"] &
        S (pkg_config_flags "ocaml-freestanding")
  | _ -> ()
  end
