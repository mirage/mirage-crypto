open Mirage_crypto.Uncommon
open Common

type pub = { g : Z.t ; n : Z.t ; n2: Z.t}

type priv = {g:Z.t; n:Z.t ; n2:Z.t ; p: Z.t ; q: Z.t ; lambda: Z.t ; mu: Z.t }

let two = Z.(~$2)

let minimum_octets = 12
let minimum_bits = 8 * minimum_octets - 7

let pub ~g ~n  =
    let n2 = Z.(mul n n) in
    let* () = guard ( g < n2 ) ( `Msg "g is greater than n^2" ) in
    let* () = guard Z.( n > zero && numbits n >= minimum_bits) ( `Msg "invalid nZ" ) in
    Ok {g; n; n2}

let priv ~p ~q =
    let* () = valid_prime "p" p in
    let* () = valid_prime "q" q in
    let* () = guard (p <> q) (`Msg "p and q are the same number") in
    let* () = guard Z.((lcm (mul p q) (mul (pred p) (pred q)))=one) (`Msg "gcd(p.q, (p−1).(q−1)) = 1") in
    let n = Z.(mul p q) in
    let g = Z.(succ n) in
    let n2 = Z.(mul n n) in
    let lambda = Z.(lcm (pred p) (pred q)) in
    let mu = Z.(invert lambda n) in
    let _ = pub ~g ~n in
    Ok {g; n; n2; p; q; lambda; mu}

let pub_of_priv ({g; n; n2; _}: priv) = {g; n; n2}

let pub_bits  ({ n; _ } : pub)  = Z.numbits n
and priv_bits ({ n; _ } : priv) = Z.numbits n

let random_bigint_range ~lower ~upper =
    let diff = Z.sub upper lower in
    Z.(add lower (Random.State.bits (Random.get_state ()) |> of_int) |> rem diff)

let rec generate ?g ~bits ()=
    if bits < minimum_bits then
        invalid_arg "Paillier.generate: bits: %d < minimum_bits: %d" bits minimum_bits;
    let (pb, qb) = (bits / 2, bits - bits / 2) in
    let (p, q) = Z_extra.(prime ?g ~msb:2 pb, prime ?g ~msb:2 qb) in
    match priv ~p ~q with
    | Error _ -> generate ?g ~bits ()
    | Ok priv_key -> pub_of_priv priv_key, priv_key

let encrypt ~(pub_key : pub) ~msg ?(r=random_bigint_range ~lower:Z.one ~upper:Z.(pred pub_key.n)) () =
    if msg < Z.zero then
        invalid_arg "Paillier.encrypt: msg must be non-negative";
    let gm = Z.powm pub_key.g msg pub_key.n2 in
    let rn = Z.powm r pub_key.n pub_key.n2 in
    Z.(rem (mul gm rn) pub_key.n2)

let decrypt ~(priv_key : priv) ~c =
    let cn = Z.powm c priv_key.lambda priv_key.n2 in
    let lx = Z.(div (sub cn one) priv_key.n) in
    Z.(rem (mul priv_key.mu lx) priv_key.n)
