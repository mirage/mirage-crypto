let evar  = "MIRAGE_CRYPTO_ACCELERATE"
let needs = [`SSSE3; `AES; `PCLMULQDQ]
let flags = ["-DACCELERATE"; "-mssse3"; "-maes"; "-mpclmul"]
let std_flags = ["--std=c99"; "-Wall"; "-Wextra"; "-O3"]

let _ =
  let auto = match Cpuid.supports needs with Ok true -> flags | _ -> [] in
  let accelerate_flags = match Sys.getenv evar with
    | "true" -> flags
    | "false" -> []
    | _ -> auto
    | exception Not_found -> auto
  in
  let fs = std_flags @ accelerate_flags in
  Format.(printf "(@[%a@])@.%!" (fun ppf -> List.iter (fprintf ppf "%s@ ")) fs)
