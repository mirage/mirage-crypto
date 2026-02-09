# OCaml cryptographic primitives library

This repository contains a small cryptographic library that puts emphasis on the
applicative style and ease of use. It includes basic ciphers (AES, 3DES, RC4,
ChaCha20/Poly1305), AEAD primitives (AES-GCM, AES-CCM, ChaCha20/Poly1305),
public-key primitives (RSA, DSA, DH), elliptic curves (NIST P-256, P-384, P-521,
and curve 25519), and a strong RNG (Fortuna).

RSA timing attacks are countered by blinding. AES timing attacks are avoided by
delegating to AES-NI.

Initially, this package was developed for [MirageOS](https://mirageos.org), but
it is very well suitable in any OCaml application. It is a fork of the
[ocaml-nocrypto](https://github.com/mirleft/ocaml-nocrypto) package developed by
David Kaloper. It was forked with the permission of the original author in order
to facilitate changes (e.g. build system) required by MirageOS that the upstream
didn't have time to keep up with.

The following packages are provided in this repository, each installable via `opam install package-name`:
- `mirage-crypto` - the base for symmetric ciphers (AES, 3DES, RC4, ChaCha20/Poly1305) - uses allocation-free and loop-free C code,
- `mirage-crypto-ec` - elliptic curves (NIST P-256, P-384, P-521, curve 25519) - uses primitives exported from [fiat-crypto](https://github.com/mit-plv/fiat-crypto),
- `mirage-crypto-pk` - asymmetric cryptography (RSA, DSA, DH) - uses [zarith](https://github.com/ocaml/zarith) with [gmp](https://gmplib.org),
- `mirage-crypto-rng` - random number generators (Fortuna, HMAC-DRBG),
- `mirage-crypto-rng-mirage` - seed and feed entropy for the RNG with MirageOS unikernels,
- `mirage-crypto-rng-miou-unix` - seed and feed entropy for the RNG with [miou](https://github.com/robur-coop/miou) scheduler on Unix.

On Unix, `getrandom()` (and `getentropy()`) or `/dev/urandom` are used for
random number generation. On MirageOS, entropy harvesting and feeding uses
non-deterministic execution time ([whirlwind RNG](https://www.ieee-security.org/TC/SP2014/papers/Not-So-RandomNumbersinVirtualizedLinuxandtheWhirlwindRNG.pdf)),
and hooks into the main event loop to get some bits of the timestamp of each event,
`rdrand` and `rdseed` CPU instructions if available.

## RNG seeding

If RNG fails with `Fatal error: exception Unseeded_generator`, you need to
seed it.

```OCaml
let () = Mirage_crypto_rng_unix.use_default ()
```

## LICENSE

Mostly ISC, the `mirage-crypto-rng-mirage` is 2 clause BSD licensed, `mirage-crypto-ec` is MIT licensed.
