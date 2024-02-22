let () =
  let c = Configurator.V1.create "mirage-crypto" in
  let ccomp_type_opt = Configurator.V1.ocaml_config_var c "ccomp_type" in
  let arch =
    let defines =
      Configurator.V1.C_define.import
        c
        ~includes:[]
        [("__x86_64__", Switch); ("__i386__", Switch); ("__powerpc64__", Switch);
         ("__s390x__", Switch); ("__aarch64__", Switch);
         ("_WIN64", Switch); ("_WIN32", Switch)]
    in
    match defines with
    | (_, Switch true) :: _ -> `x86_64
    | _ :: (_, Switch true) :: _ -> `x86
    | _ :: _ :: (_, Switch true) :: _ -> `ppc64
    | _ :: _ :: _ :: (_, Switch true) :: _ -> `s390x
    | _ :: _ :: _ :: _ :: (_, Switch true) :: _ -> `arm64
    | _ :: _ :: _ :: _ :: _ :: (_, Switch true) :: _ -> `x86_64
    | _ :: _ :: _ :: _ :: _ :: _ :: (_, Switch true) :: _ -> `x86
    | _ -> `unknown
  in
  let os =
    let defines =
      Configurator.V1.C_define.import
        c
        ~includes:[]
        [("__APPLE__", Switch)]
    in
    match defines with
    | (_, Switch true) :: _ -> `macos
    | _ -> `unknown
  in
  let accelerate_flags =
    match arch, ccomp_type_opt with
    | `x86_64, Some "msvc" -> [ "-DACCELERATE" ]
    | `x86_64, _ -> [ "-DACCELERATE"; "-mssse3"; "-maes"; "-mpclmul" ]
    | _ -> []
  in
  let ent_flags =
    match arch, ccomp_type_opt with
    | (`x86_64 | `x86), Some "msvc" -> [ "-DENTROPY" ]
    | (`x86_64 | `x86), _ -> [ "-DENTROPY"; "-mrdrnd"; "-mrdseed" ]
    | _ -> []
  in
  let std_flags =
    match ccomp_type_opt with
    | Some "msvc" -> ["/Wall"]
    | _ -> ["--std=c11"; "-Wall"; "-Wextra"; "-Wpedantic"; "-O3"]
  in
  let warn_flags =
    (* See #178, there may be false positives on ppc&s390 with no-stringop-overflow *)
    match arch, ccomp_type_opt with
    | _, Some "msvc" -> [ "/WX" ]
    | (`ppc64, _) | (`s390x, _) -> [ "-Wno-stringop-overflow"; "-Werror" ]
    | _ -> [ "-Werror" ]
  in
  let no_instcombine_on_macos = match arch, os with
    | `arm64, `macos ->
      let res = Configurator.V1.Process.run c "cc" ["-dumpversion"] in
      if String.trim res.stdout = "14.0.3" then
        ["-mllvm"; "--instcombine-max-iterations=0"]
        (* macOS instcombine miscompilation with clang 14.0.3 *)
      else
        []
    | _ -> []
  in
  let flags = std_flags @ no_instcombine_on_macos @ ent_flags in
  let opt_flags = flags @ accelerate_flags in
  Configurator.V1.Flags.write_sexp "cflags_optimized.sexp" opt_flags;
  Configurator.V1.Flags.write_sexp "cflags.sexp" flags;
  Configurator.V1.Flags.write_sexp "cflags_warn.sexp" warn_flags
