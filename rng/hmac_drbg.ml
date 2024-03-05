module Make (H : Digestif.S) = struct
  type g =
    { mutable k      : string
    ; mutable v      : string
    ; mutable seeded : bool
    }

  let block = H.digest_size

  let b x = String.make 1 (char_of_int x)

  let (bx00, bx01) = b 0x00, b 0x01

  let k0 = String.make H.digest_size '\x00'
  and v0 = String.make H.digest_size '\x01'

  let create ?time:_ () = { k = k0 ; v = v0 ; seeded = false }

  let seeded ~g = g.seeded

  let reseed ~g buf =
    let (k, v) = (g.k, g.v) in
    let k = H.hmac_string ~key:k @@ String.concat "" [v; bx00; buf] |> H.to_raw_string in
    let v = H.hmac_string ~key:k v |> H.to_raw_string in
    let k = H.hmac_string ~key:k @@ String.concat "" [v; bx01; buf] |> H.to_raw_string in
    let v = H.hmac_string ~key:k v |> H.to_raw_string in
    g.k <- k ; g.v <- v ; g.seeded <- true

  let generate ~g bytes =
    if not g.seeded then raise Rng.Unseeded_generator ;
    let rec go acc k v = function
      | 0 -> (v, String.concat "" @@ List.rev acc)
      | i ->
        let v = H.hmac_string ~key:k v |> H.to_raw_string in
        go (v::acc) k v (pred i) in
    let (v, buf) = go [] g.k g.v Mirage_crypto.Uncommon.(bytes // H.digest_size) in
    g.k <- H.hmac_string ~key:g.k (v ^ bx00) |> H.to_raw_string;
    g.v <- H.hmac_string ~key:g.k v |> H.to_raw_string;
    String.sub buf 0 (Mirage_crypto.Uncommon.imax 0 bytes)

  (* XXX *)
  let accumulate ~g:_ = invalid_arg "Implement Hmac_drbg.accumulate..."

  let pools = 0
end
