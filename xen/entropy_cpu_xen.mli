
exception Instruction_unavailable of string
exception Entropy_unavailable

val rdtscp : unit -> int = "caml_rdtscp" "noalloc"
val rdrand : unit -> int = "caml_rdrand"
val rdseed : unit -> int = "caml_rdseed"
val rand   : unit -> int
