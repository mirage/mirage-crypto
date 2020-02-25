#!/bin/sh
flags="-O3 -std=c99 -Wall -Wpedantic"
case $(uname -m) in
    x86_64 | amd64 | x86)
        flags="$flags -mrdrnd -mrdseed"
        ;;
    *)
        ;;
esac
echo "($flags)"
