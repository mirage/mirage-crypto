#!/bin/sh

set -ex

opam install --confirm-level=unsafe-yes "mirage>4"
(cd mirage && mirage configure -t unix && gmake depend && mirage build && dist/crypto-test && cd ..) || exit 1
(cd mirage && mirage configure -t hvt && gmake depend && mirage build && cd ..) || exit 1
if [ $(uname -m) = "amd64" ] || [ $(uname -m) = "x86_64" ]; then
    (cd mirage && mirage configure -t xen && gmake depend && mirage build && cd ..) || exit 1
fi
