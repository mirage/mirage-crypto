let start () =
  Logs.info (fun m -> m "using Fortuna, entropy sources: %a"
                Fmt.(list ~sep:(any ", ") Mirage_crypto_rng.Entropy.pp_source)
                (Mirage_crypto_rng.Entropy.sources ())) ;
  Logs.info (fun m -> m "64 byte random:@ %a" (Ohex.pp_hexdump ())
                (Mirage_crypto_rng.generate 64)) ;
  let n = Bytes.(unsafe_to_string (create 32)) in
  let key = Mirage_crypto.Chacha20.of_secret n
  and nonce = Bytes.(unsafe_to_string (create 12))
  in
  Logs.info (fun m -> m "Chacha20/Poly1305 of 32*0, key 32*0, nonce 12*0: %a"
                (Ohex.pp_hexdump ())
                (Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce n));
  let key = Mirage_crypto_pk.Rsa.generate ~bits:4096 () in
  let signature =
    Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message n)
  in
  let verified =
    let key = Mirage_crypto_pk.Rsa.pub_of_priv key in
    let hashp = function `SHA256 -> true | _ -> false in
    Mirage_crypto_pk.Rsa.PKCS1.verify ~hashp ~key ~signature (`Message n)
  in
  Logs.info (fun m -> m "Generated a RSA key of %d bits (sign + verify %B)"
                (Mirage_crypto_pk.Rsa.priv_bits key) verified);
  Lwt.return_unit
