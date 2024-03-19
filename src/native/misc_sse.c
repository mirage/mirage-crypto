#include "mirage_crypto.h"

#ifdef __mc_ACCELERATE__

static inline void xor_into (const uint8_t *src, uint8_t *dst, size_t n) {
/* see issue #70 #81 for alignment considerations (memcpy used below) */
#ifdef ARCH_64BIT
  __m128i r;
  for (; n >= 16; n -= 16, src += 16, dst += 16)
    _mm_storeu_si128 (
        (__m128i*) dst,
        _mm_xor_si128 (
          _mm_loadu_si128 ((__m128i*) memcpy(&r, src, 16)),
          _mm_loadu_si128 ((__m128i*) dst)));

  uint64_t s;
  for (; n >= 8; n -= 8, src += 8, dst += 8)
    *(uint64_t*) dst ^= *(uint64_t*) memcpy(&s, src, 8);
#endif

  uint32_t t;
  for (; n >= 4; n -= 4, src += 4, dst += 4)
    *(uint32_t*) dst ^= *(uint32_t*)memcpy(&t, src, 4);

  for (; n --; ++ src, ++ dst) *dst = *src ^ *dst;
}

/* The GCM counter. Counts on the last 32 bits, ignoring carry. */
static inline void _mc_count_16_be_4 (uint64_t *init, uint64_t *dst, size_t blocks) {

  __m128i ctr, c1   = _mm_set_epi32 (1, 0, 0, 0),
               mask = _mm_set_epi64x (0x0c0d0e0f0b0a0908, 0x0706050403020100);
  ctr = _mm_shuffle_epi8 (_mm_loadu_si128 ((__m128i *) init), mask);
  for (; blocks --; dst += 2) {
    _mm_storeu_si128 ((__m128i *) dst, _mm_shuffle_epi8 (ctr, mask));
    ctr = _mm_add_epi32 (ctr, c1);
  }
}

#endif /* __mc_ACCELERATE__ */

CAMLprim value
mc_xor_into_bytes (value b1, value off1, value b2, value off2, value n) {
  _mc_switch_accel(ssse3,
    mc_xor_into_bytes_generic(b1, off1, b2, off2, n),
    xor_into (_st_uint8_off (b1, off1), _bp_uint8_off (b2, off2), Int_val (n)))
  return Val_unit;
}

#define __export_counter(name, f)                                        \
  CAMLprim value name (value ctr, value dst, value blocks) {  \
    _mc_switch_accel(ssse3,                                     \
      name##_generic (ctr, dst, blocks),                            \
      f ( (uint64_t*) Bp_val (ctr),                                      \
          (uint64_t*) _bp_uint8 (dst), Long_val (blocks) ))     \
    return Val_unit;                                                     \
  }

__export_counter(mc_count_16_be_4, _mc_count_16_be_4)

CAMLprim value mc_misc_mode (__unit ()) {
  value enabled = 0;
  _mc_switch_accel(ssse3,
    enabled = 0,
    enabled = 1)
  return Val_int (enabled);
}
