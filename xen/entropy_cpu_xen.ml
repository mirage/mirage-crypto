exception Instruction_unavailable of string
exception Entropy_unavailable

let _ =
  List.iter (fun (id, exn) -> Callback.register_exception id exn)
  [ ("entropy instruction unavailable", Instruction_unavailable "")
  ; ("entropy pool empty", Entropy_unavailable)
  ]


external rdtscp : unit -> int = "caml_rdtscp" "noalloc"
external rdrand : unit -> int = "caml_rdrand"
external rdseed : unit -> int = "caml_rdseed"
