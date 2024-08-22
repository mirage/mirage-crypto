let rec until p f = let r = f () in if p r then r else until p f

let guard p err = if p then Ok () else Error err

let valid_prime name p =
  guard Z.(p > zero && is_odd p && Z_extra.pseudoprime p)
    (`Msg ("invalid prime " ^ name))

let rprime a b = Z.(gcd a b = one)

let ( let* ) = Result.bind
