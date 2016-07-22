#!/bin/sh

export PKG_CONFIG_PATH=$(opam config var prefix)/lib/pkgconfig

pkg-config --exists $1
if [ $? -eq 0 ];
then pkg-config --static $1 --cflags
else echo ""
fi

