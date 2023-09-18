let std_flags = ["--std=c11"; "-Wall"; "-Wextra"; "-Wpedantic"]

let () =
  let c = Configurator.V1.create "mirage-crypto" in
  let arch =
    let defines =
      Configurator.V1.C_define.import
        c
        ~includes:[]
        [("__x86_64__", Switch); ("__i386__", Switch); ("__powerpc64__", Switch);
         ("__s390x__", Switch); ("__aarch64__", Switch)]
    in
    match defines with
    | (_, Switch true) :: _ -> `x86_64
    | _ :: (_, Switch true) :: _ -> `x86
    | _ :: _ :: (_, Switch true) :: _ -> `ppc64
    | _ :: _ :: _ :: (_, Switch true) :: _ -> `s390x
    | _ :: _ :: _ :: _ :: (_, Switch true) :: _ -> `arm64
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
    match arch with
    | `x86_64 -> [ "-DACCELERATE"; "-mssse3"; "-maes"; "-mpclmul" ]
    | _ -> []
  in
  let ent_flags =
    match arch with
    | `x86_64 | `x86 -> [ "-DENTROPY"; "-mrdrnd"; "-mrdseed" ]
    | _ -> []
  in
  let warn_flags =
    (* See #178, there may be false positives on ppc&s390 with no-stringop-overflow *)
    match arch with
    | `ppc64 | `s390x -> [ "-Wno-stringop-overflow"; "-Werror" ]
    | _ -> [ "-Werror" ]
  in
  let optimization = match arch, os with
    | `arm64, `macos ->
      let res = Configurator.V1.Process.run c "cc" ["-dumpversion"] in
      if String.trim res.stdout = "14.0.3" then
        "-O0" (* macOS instcombine miscompilation with clang 14.0.3 *)
      else
        "-O3"
    | _ -> "-O3"
  in
  let flags = optimization :: std_flags @ ent_flags in
  let opt_flags = flags @ accelerate_flags in
  Configurator.V1.Flags.write_sexp "cflags_optimized.sexp" opt_flags;
  Configurator.V1.Flags.write_sexp "cflags.sexp" flags;
  Configurator.V1.Flags.write_sexp "cflags_warn.sexp" warn_flags
