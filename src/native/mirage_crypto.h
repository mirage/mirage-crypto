#if !defined (H__MIRAGE_CRYPTO)
#define H__MIRAGE_CRYPTO

/* endian.h is tricky (not specified), inspiration from;
   https://gist.github.com/panzi/6856583 (public domain)
   and https://gist.github.com/PkmX/63dd23f28ba885be53a5 (also public domain)
*/
#if defined(__freestanding__)
#include <endian.h>

#elif defined(__FreeBSD__) || defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
#include <sys/endian.h>

#elif defined(__APPLE__)
#include <libkern/OSByteOrder.h>
#define htobe16(x) OSSwapHostToBigInt16(x)
#define htole16(x) OSSwapHostToLittleInt16(x)
#define be16toh(x) OSSwapBigToHostInt16(x)
#define le16toh(x) OSSwapLittleToHostInt16(x)
#define htobe32(x) OSSwapHostToBigInt32(x)
#define htole32(x) OSSwapHostToLittleInt32(x)
#define be32toh(x) OSSwapBigToHostInt32(x)
#define le32toh(x) OSSwapLittleToHostInt32(x)
#define htobe64(x) OSSwapHostToBigInt64(x)
#define htole64(x) OSSwapHostToLittleInt64(x)
#define be64toh(x) OSSwapBigToHostInt64(x)
#define le64toh(x) OSSwapLittleToHostInt64(x)
#define bswap16(x) OSSwapInt16(x)
#define bswap32(x) OSSwapInt32(x)
#define bswap64(x) OSSwapInt64(x)

#elif defined(__linux__)
#define __USE_MISC
#define _GNU_SOURCE
#define _DEFAULT_SOURCE
#include <endian.h>
#include <byteswap.h>
#define bswap16(x) bswap_16(x)
#define bswap32(x) bswap_32(x)
#define bswap64(x) bswap_64(x)

#elif (defined(__WINDOWS__) || defined(_WIN32) || defined(_WIN64))
#include <windows.h>
#if BYTE_ORDER == LITTLE_ENDIAN
#if defined(_MSC_VER)
#include <stdlib.h>
#define htobe16(x) _byteswap_ushort(x)
#define htole16(x) (x)
#define be16toh(x) _byteswap_ushort(x)
#define le16toh(x) (x)
#define htobe32(x) _byteswap_ulong(x)
#define htole32(x) (x)
#define be32toh(x) _byteswap_ulong(x)
#define le32toh(x) (x)
#define htobe64(x) _byteswap_uint64(x)
#define htole64(x) (x)
#define be64toh(x) _byteswap_uint64(x)
#define le64toh(x) (x)

#define bswap16(x) _byteswap_ushort(x)
#define bswap32(x) _byteswap_ulong(x)
#define bswap64(x) _byteswap_uint64(x)

#elif defined(__GNUC__) || defined(__clang__)

#define htobe16(x) __builtin_bswap16(x)
#define htole16(x) (x)
#define be16toh(x) __builtin_bswap16(x)
#define le16toh(x) (x)
#define htobe32(x) __builtin_bswap32(x)
#define htole32(x) (x)
#define be32toh(x) __builtin_bswap32(x)
#define le32toh(x) (x)
#define htobe64(x) __builtin_bswap64(x)
#define htole64(x) (x)
#define be64toh(x) __builtin_bswap64(x)
#define le64toh(x) (x)

#define bswap16(x) __builtin_bswap16(x)
#define bswap32(x) __builtin_bswap32(x)
#define bswap64(x) __builtin_bswap64(x)

#else /* MSC / GNUC / clang */
#error "windows platform not supported (only MSC and gcc/clang)"
#endif

#else /* BYTE_ORDER == BIG_ENDIAN */
#error "big endian byte order not supported on windows"
#endif

#else /* all OS defines */
#error "unsupported platform"
#endif

#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#if defined (__x86_64__) && defined (ACCELERATE)
#include <x86intrin.h>
#endif

#if defined (__x86_64__) && defined (ACCELERATE) && defined (__SSSE3__)
#define __mc_SSE__
#endif

#if defined (__x86_64__) && defined (ACCELERATE) && defined (__AES__)
#define __mc_AES_NI__
#else
#define __mc_AES_GENERIC__
#endif

#if defined (__x86_64__) && defined (ACCELERATE) && defined (__PCLMUL__)
#define __mc_PCLMUL__
#endif

#ifndef __unused
#define __unused(x) x __attribute__((unused))
#endif
#define __unit() value __unused(_)

#define _ba_uint8_off(ba, off)  ((uint8_t*) Caml_ba_data_val (ba) + Long_val (off))
#define _ba_uint32_off(ba, off) ((uint32_t*) Caml_ba_data_val (ba) + Long_val (off))

#define _ba_uint8(ba)  ((uint8_t*) Caml_ba_data_val (ba))
#define _ba_uint32(ba) ((uint32_t*) Caml_ba_data_val (ba))

#define _bp_uint8_off(bp, off) ((uint8_t *) Bp_val (bp) + Long_val (off))
#define _bp_uint8(bp) ((uint8_t *) Bp_val (bp))
#define _bp_uint32(bp) ((uint32_t *) Bp_val (bp))

#define __define_bc_6(f) \
  CAMLprim value f ## _bc (value *v, int __unused(c) ) { return f(v[0], v[1], v[2], v[3], v[4], v[5]); }

#define __define_bc_7(f) \
  CAMLprim value f ## _bc (value *v, int __unused(c) ) { return f(v[0], v[1], v[2], v[3], v[4], v[5], v[6]); }

#endif /* H__MIRAGE_CRYPTO */
