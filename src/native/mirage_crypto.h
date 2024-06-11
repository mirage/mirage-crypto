#if !defined (H__MIRAGE_CRYPTO)
#define H__MIRAGE_CRYPTO

#include <stdint.h>
#include <string.h>
#include "bitfn.h"

#include <caml/mlvalues.h>

#ifdef ACCELERATE
# ifdef _MSC_VER
#  include <intrin.h>
# else
#  include <x86intrin.h>
# endif
#define __mc_ACCELERATE__
#define __mc_detect_features__
#endif

#ifdef ENTROPY
#define __mc_ENTROPY__
#define __mc_detect_features__
#endif

#ifdef __mc_detect_features__

struct _mc_cpu_features {
  int aesni;
  int pclmul;
  int ssse3;
  int rdrand;
  int rdseed;
};

/* Supported accelerations */
extern struct _mc_cpu_features mc_detected_cpu_features;

#endif /* __mc_detect_features__ */

#ifdef __mc_ACCELERATE__

#define _mc_switch_accel(FEATURE, GENERIC_CALL, ACCELERATED_CALL) \
  if (!(mc_detected_cpu_features.FEATURE)) { GENERIC_CALL; } \
  else { ACCELERATED_CALL; }

#else /* __mc_ACCELERATE__ */

#define _mc_switch_accel(_FEATURE, GENERIC_CALL, _ACCELERATED_CALL) \
  GENERIC_CALL;

#endif /* __mc_ACCELERATE__ */

#if defined (__x86_64__) || defined (__aarch64__) || defined (__powerpc64__) || (64 == __riscv_xlen) || defined (__s390x__) || (defined (__mips__) && _MIPS_SIM==_ABI64) || defined (__loongarch_lp64) || (1 == _WIN64)
#define ARCH_64BIT
#elif defined (__i386__) || defined (__arm__) || (32 == __riscv_xlen) || (defined (__mips__) && _MIPS_SIM==_ABIO32) || (1 == _WIN32)
#define ARCH_32BIT
#else
#error "unsupported platform"
#endif

#ifndef __unused
# if defined(_MSC_VER) && _MSC_VER >= 1500
#  define __unused(x) __pragma( warning (push) ) \
    __pragma( warning (disable:4189 ) ) \
    x \
    __pragma( warning (pop))
# else
#  define __unused(x) x __attribute__((unused))
# endif
#endif
#define __unit() value __unused(_)

#define _st_uint8(v) ((const uint8_t*) (String_val(v)))
#define _st_uint32(v) ((const uint32_t*) (String_val(v)))
#define _st_uint8_off(v, off) ((const uint8_t*)(String_val(v) + Long_val(off)))

#define _bp_uint8_off(bp, off) ((uint8_t *) Bp_val (bp) + Long_val (off))
#define _bp_uint8(bp) ((uint8_t *) Bp_val (bp))
#define _bp_uint32(bp) ((uint32_t *) Bp_val (bp))

#define __define_bc_6(f) \
  CAMLprim value f ## _bc (value *v, int __unused(c) ) { return f(v[0], v[1], v[2], v[3], v[4], v[5]); }

#define __define_bc_7(f) \
  CAMLprim value f ## _bc (value *v, int __unused(c) ) { return f(v[0], v[1], v[2], v[3], v[4], v[5], v[6]); }

/* Signature of generic functions */

CAMLprim value mc_aes_rk_size_generic (value rounds);

CAMLprim value
mc_aes_derive_e_key_generic (value key, value rk, value rounds);

CAMLprim value
mc_aes_derive_d_key_generic (value key, value kr, value rounds, value __unused (rk));

CAMLprim value
mc_aes_enc_generic (value src, value off1, value dst, value off2, value rk, value rounds, value blocks);

CAMLprim value
mc_aes_dec_generic (value src, value off1, value dst, value off2, value rk, value rounds, value blocks);

CAMLprim value mc_ghash_key_size_generic (__unit ());

CAMLprim value mc_ghash_init_key_generic (value key, value m);

CAMLprim value
mc_ghash_generic (value m, value hash, value src, value off, value len);

CAMLprim value
mc_xor_into_generic (value b1, value off1, value b2, value off2, value n);

CAMLprim value
mc_xor_into_bytes_generic (value b1, value off1, value b2, value off2, value n);

CAMLprim value
mc_count_16_be_4_generic (value ctr, value dst, value off, value blocks);

#endif /* H__MIRAGE_CRYPTO */
