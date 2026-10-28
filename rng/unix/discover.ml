let () =
  let open Configurator.V1 in
  main ~name:"rng_flags" (fun t ->
    let c_lib_flags =
      match ocaml_config_var t "system" with
      | Some ("mingw" | "mingw64" | "win32" | "win64" | "cygwin") -> ["-lbcrypt"]
      | _ -> []
    in
    Flags.write_sexp "rng_c_flags.sexp" c_lib_flags
  )
