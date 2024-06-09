open Mirage

let main =
  let packages = [
    package "mirage-crypto-rng" ;
    package "mirage-crypto-pk" ;
    package "mirage-crypto" ;
    package ~min:"0.8.7" "fmt" ;
    package "ohex" ;
  ]
  in
  main ~packages "Unikernel.Main" (random @-> job)

let () =
  register "crypto-test" [main $ default_random]
