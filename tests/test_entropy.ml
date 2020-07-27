
let data = ref Cstruct.empty

let bootstrap_check () =
  for _i = 0 to 10 do
    let data' = Mirage_crypto_rng.Entropy.bootstrap 1 in
    if Cstruct.equal !data data' then failwith "same data from bootstrap" ;
    data := data'
  done

let timer_check () =
  let data' = Mirage_crypto_rng.Entropy.interrupt_hook () () in
  data := Cstruct.create (Cstruct.len data');
  for _i = 0 to 10 do
    let data' = Mirage_crypto_rng.Entropy.interrupt_hook () () in
    if Cstruct.equal !data data' then failwith "same data from timer" ;
    Cstruct.blit data' 0 !data 0 (Cstruct.len data')
  done

let () =
  timer_check ();
  bootstrap_check ();
  print_endline "test entropy OK"
