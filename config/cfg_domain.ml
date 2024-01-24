let parse s = Scanf.sscanf s "%d.%d" (fun major minor -> (major, minor))

let () =
  let version = parse Sys.ocaml_version in
  if version >= (5, 0)
  then print_string "domain.stable.ml"
  else print_string "domain.pre500.ml"
