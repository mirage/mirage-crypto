/* Based on https://github.com/abeaumont/ocaml-chacha.git */

#include "mirage_crypto.h"

static inline void mc_chacha_quarterround(uint32_t *x, int a, int b, int c, int d) {
  x[a] += x[b]; x[d] = rol32(x[d] ^ x[a], 16);
  x[c] += x[d]; x[b] = rol32(x[b] ^ x[c], 12);
  x[a] += x[b]; x[d] = rol32(x[d] ^ x[a], 8);
  x[c] += x[d]; x[b] = rol32(x[b] ^ x[c], 7);
}

void mc_chacha_core_generic(int count, const uint32_t *src, uint32_t *dst) {
  uint32_t x[16];
  cpu_to_le32_array(x, src, 16);
  for (int i = 0; i < count; i++) {
    mc_chacha_quarterround(x, 0, 4, 8, 12);
    mc_chacha_quarterround(x, 1, 5, 9, 13);
    mc_chacha_quarterround(x, 2, 6, 10, 14);
    mc_chacha_quarterround(x, 3, 7, 11, 15);

    mc_chacha_quarterround(x, 0, 5, 10, 15);
    mc_chacha_quarterround(x, 1, 6, 11, 12);
    mc_chacha_quarterround(x, 2, 7, 8, 13);
    mc_chacha_quarterround(x, 3, 4, 9, 14);
  }
  for (int i = 0; i < 16; i++) {
    uint32_t xi = x[i];
    uint32_t hj = cpu_to_le32(src[i]);
    dst[i] = le32_to_cpu(xi + hj);
  }
}

