#include "mirage_crypto.h"

#if defined (__i386__) || defined (__x86_64__)

#include <cpuid.h>

struct _mc_cpu_features mc_detected_cpu_features = { 0 };

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  unsigned int eax = 0, ebx = 0, ecx = 0, edx = 0;

  if (__get_cpuid(1, &eax, &ebx, &ecx, &edx)) {
    if (ecx & bit_PCLMUL)
      mc_detected_cpu_features.pclmul = 1;
    if (ecx & bit_SSSE3)
      mc_detected_cpu_features.ssse3 = 1;
    if (ecx & bit_AES)
      mc_detected_cpu_features.aesni = 1;
    if (ecx & bit_RDRND)
      mc_detected_cpu_features.rdrand = 1;
  }

  if (__get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx)) {
    if (ebx & bit_RDSEED)
      mc_detected_cpu_features.rdseed = 1;
  }

  return Val_unit;
}

#else /* i386 || x86_64 */

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  return Val_unit;
}

#endif /* i386 || x86_64 */
