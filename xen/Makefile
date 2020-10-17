PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists mirage-xen-posix,mirage-xen-ocaml; echo $$?)

.PHONY: all clean
all: libmirage_crypto_xen_stubs.a

ifeq ($(EXISTS), 1)
libmirage_crypto_xen_stubs.a:
	touch $@
else
CC ?= cc
XEN_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags mirage-xen-posix,mirage-xen-ocaml)
MIRAGE_CRYPTO_CFLAGS := $(shell sed 's/^(\(.*\))$$/\1/' ../src/cflags.sexp | tr -d '"')
CFLAGS := $(MIRAGE_CRYPTO_CFLAGS) $(XEN_CFLAGS)

OBJS=detect_cpu_features.o misc.o misc_sse.o hash_stubs.o md5.o sha1.o \
  sha256.o sha512.o aes_generic.o aes_aesni.o des_generic.o chacha.o \
  poly1305-donna.o ghash_pclmul.o ghash_generic.o ghash_ctmul.o \
  entropy_cpu_stubs.o

libmirage_crypto_xen_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libmirage_crypto_xen_stubs.a
