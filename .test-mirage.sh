#!/bin/sh

set -ex

opam install -y mirage
(cd mirage && mirage configure -t unix && make depend && mirage build && ./crypto_test && mirage clean && cd ..) || exit 1
(cd mirage && mirage configure -t hvt && make depend && mirage build && mirage clean && cd ..) || exit 1
