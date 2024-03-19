module Make (H : Digestif.S) = struct
  type g =
    { mutable k      : string
    ; mutable v      : string
    ; mutable seeded : bool
    }

  let block = H.digest_size

  let (bx00, bx01) = "\x00", "\x01"

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

  let generate_into ~g buf ~off len =
    if not g.seeded then raise Rng.Unseeded_generator ;
    let rec go off k v = function
      | 0 -> v
      | 1 ->
        let v = H.hmac_string ~key:k v |> H.to_raw_string in
        let len =
          let rem = len mod H.digest_size in
          if rem = 0 then H.digest_size else rem
        in
        Bytes.unsafe_blit_string v 0 buf off len;
        v
      | i ->
        let v = H.hmac_string ~key:k v |> H.to_raw_string in
        Bytes.unsafe_blit_string v 0 buf off H.digest_size;
        go (off + H.digest_size) k v (pred i)
    in
    let v = go off g.k g.v Mirage_crypto.Uncommon.(len // H.digest_size) in
    g.k <- H.hmac_string ~key:g.k (v ^ bx00) |> H.to_raw_string;
    g.v <- H.hmac_string ~key:g.k v |> H.to_raw_string

  (* XXX *)
  let accumulate ~g:_ = invalid_arg "Implement Hmac_drbg.accumulate..."

  let pools = 0
end
