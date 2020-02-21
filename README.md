# mirage-crypto - Cryptographic primitives for MirageOS

%%VERSION%%

mirage-crypto is a small cryptographic library that puts emphasis on the
applicative style and ease of use. It includes basic ciphers (AES, 3DES, RC4),
hashes (MD5, SHA1, SHA2 family), AEAD primitives (AES-GCM, AES-CCM), public-key
primitives (RSA, DSA, DH) and a strong RNG (Fortuna).

RSA timing attacks are countered by blinding. AES timing attacks are avoided by
delegating to AES-NI.

Mirage-crypto is a fork of the
[ocaml-nocrypto](https://github.com/mirleft/ocaml-nocrypto) written by David
Kaloper.  It was forked with the permission of the original author in order to
facilitate changes (e.g. build system) required by Mirage that the upstream
didn't have time to keep up with.

# Status

This is a work in progress repository that is not yet released.  Please see
https://github.com/mirage/mirage-crypto/issues/1 for our progress towards a
stable release.

## Build

```bash
dune build
dune runtest
```

## FAQ

#### RNG seeding

If RNG fails with `Fatal error: exception Uncommon.Boot.Unseeded_generator`, you
need to [seed][doc-entropy] it.

Unix:
```OCaml
let () = Mirage_crypto_rng_unix.initialize ()
```

[doc-entropy]: http://mirage.github.io/mirage-crypto/Mirage_crypto_rng_unix.html

#### Illegal instructions

```
Program terminated with signal SIGILL, Illegal instruction.
#0  _mm_aeskeygenassist_si128 (__C=<optimized out>, __X=...)
```

`Mirage_crypto` has CPU acceleration support (`SSE2`+`AES-NI`), but no run-time
autodetection yet. You compiled the library with acceleration, but you are using
it on a machine that does not support it.

The environment variable `MIRAGE_CRYPTO_ACCELERATE` can be used to override
detection:

- `MIRAGE_CRYPTO_ACCELERATE=false dune build` force-disables non-portable code.
- `MIRAGE_CRYPTO_ACCELERATE=true dune build` force-enables non-portable code.
- Otherwise, it matches the capabilities of the build machine.
