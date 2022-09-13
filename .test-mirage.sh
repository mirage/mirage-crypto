#!/bin/sh

set -ex

opam install --confirm-level=unsafe-yes "mirage>4"
dune subst
(mirage configure -t unix -f mirage/config.ml && gmake depend && mirage build -f mirage/config.ml && mirage/dist/crypto-test) || exit 1
(mirage configure -t hvt -f mirage/config.ml && gmake depend && mirage build -f mirage/config.ml) || exit 1
if [ $(uname -m) = "amd64" ] || [ $(uname -m) = "x86_64" ]; then
    (mirage configure -t xen -f mirage/config.ml && gmake depend && mirage build -f mirage/config.ml) || exit 1
fi
