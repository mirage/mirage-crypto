#!/bin/sh

set -ex

opam install --confirm-level=unsafe-yes "mirage>4"
# to satisfy hardcoded version constraints in mirage, we need to be < 0.12.0
# and "dune subst" doesn't work on these PR checkouts
version='version: "2.99.0~dev"'
echo $version >> mirage-crypto-rng-mirage.opam
echo $version >> mirage-crypto-rng.opam
echo $version >> mirage-crypto.opam
echo $version >> mirage-crypto-pk.opam
(mirage configure -t unix -f mirage/config.ml && gmake depend && dune build --root . mirage/dist/ && mirage/dist/crypto-test) || exit 1
(mirage configure -t hvt -f mirage/config.ml && gmake depend && dune build --root . mirage/dist/) || exit 1
if [ $(uname -m) = "amd64" ] || [ $(uname -m) = "x86_64" ]; then
    (mirage configure -t xen -f mirage/config.ml && gmake depend && dune build --root . mirage/dist/) || exit 1
fi
