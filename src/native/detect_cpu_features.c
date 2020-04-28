#include "mirage_crypto.h"

#ifdef __mc_ACCELERATE__

#include <cpuid.h>

struct _mc_cpu_features mc_detected_cpu_features = { 0 };

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  unsigned int eax = 0, ebx = 0, ecx = 0, edx = 0;

  if (__get_cpuid(1, &eax, &ebx, &ecx, &edx))
  {
    if (ecx & bit_PCLMUL)
      mc_detected_cpu_features.pclmul = 1;
    if (ecx & bit_SSSE3)
      mc_detected_cpu_features.ssse3 = 1;
    if (ecx & bit_AES)
      mc_detected_cpu_features.aesni = 1;
  }
  return Val_unit;
}

#else /* __mc_ACCELERATE__ */

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  return Val_unit;
}

#endif /* __mc_ACCELERATE__ */
