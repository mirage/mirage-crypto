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
