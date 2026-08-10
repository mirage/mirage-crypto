let data = ref ""

let cpu_bootstrap_check () =
  match Entropy.cpu_rng_bootstrap with
  | Error `Not_supported -> print_endline "no CPU RNG bootstrap"
  | Ok cpu_rng_bootstrap ->
    match cpu_rng_bootstrap 1 with
    | exception Failure f -> print_endline ("bad CPU RNG: " ^ f)
    | data' ->
      data := data';
      for i = 0 to 10 do
        try
          let data' = cpu_rng_bootstrap 1 in
          if String.equal !data data' then
            failwith ("same data from CPU bootstrap at " ^ string_of_int i ^ ": " ^ String.escaped data');
          data := data'
        with Failure f -> failwith ("CPU RNG failed at " ^ string_of_int i ^ " with " ^ f)
      done

let whirlwind_bootstrap_check () =
  for i = 0 to 10 do
    let data' = Entropy.whirlwind_bootstrap 1 in
    if String.equal !data data' then
      failwith ("same data from whirlwind bootstrap at " ^ string_of_int i ^ " with " ^ String.escaped data');
    data := data'
  done

let timer_check () =
  for i = 0 to 10 do
    let data' = Entropy.interrupt_hook () in
    if String.equal !data data' then
      failwith ("same data from timer at " ^ string_of_int i ^ " with: " ^ String.escaped data');
    data := data'
  done

let test () =
  timer_check ();
  cpu_bootstrap_check ();
  whirlwind_bootstrap_check ()
