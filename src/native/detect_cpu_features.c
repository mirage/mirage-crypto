#include "mirage_crypto.h"

#ifdef __mc_detect_features__

#ifndef _MSC_VER
# include <cpuid.h>
#endif

struct _mc_cpu_features mc_detected_cpu_features = { 0 };

#ifdef _MSC_VER
#define bit_PCLMUL ((int)1 << 1)
#define bit_SSSE3 ((int)1 << 9)
#define bit_AES ((int)1 << 25)
#define bit_RDRND ((int)1 << 30)
#define bit_RDSEED ((int)1 << 18)

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  int cpuInfo[4] = {-1};
  int ebx;
  int ecx;

  __cpuid(cpuInfo, 0x00000000);
  int max = cpuInfo[0];
  if (max < 1) return Val_unit;

  __cpuid(cpuInfo, 0x00000001);
  ecx = cpuInfo[2];

  if (ecx & bit_PCLMUL)
    mc_detected_cpu_features.pclmul = 1;
  if (ecx & bit_SSSE3)
    mc_detected_cpu_features.ssse3 = 1;
  if (ecx & bit_AES)
    mc_detected_cpu_features.aesni = 1;
  if (ecx & bit_RDRND)
    mc_detected_cpu_features.rdrand = 1;

  if (max > 7) {
    __cpuid(cpuInfo, 0x00000007);
    ebx = cpuInfo[1];
    if (ebx & bit_RDSEED)
      mc_detected_cpu_features.rdseed = 1;
  }

  return Val_unit;
}

#else

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  unsigned int sig = 0, eax = 0, ebx = 0, ecx = 0, edx = 0;

  int max = __get_cpuid_max(0, &sig);

  if (max < 1) return Val_unit;

  __cpuid(1, eax, ebx, ecx, edx);
  if (ecx & bit_PCLMUL)
    mc_detected_cpu_features.pclmul = 1;
  if (ecx & bit_SSSE3)
    mc_detected_cpu_features.ssse3 = 1;
  if (ecx & bit_AES)
    mc_detected_cpu_features.aesni = 1;
  if (ecx & bit_RDRND)
    mc_detected_cpu_features.rdrand = 1;

  if (max > 7) {
    __cpuid_count(7, 0, eax, ebx, ecx, edx);
    if (ebx & bit_RDSEED)
      mc_detected_cpu_features.rdseed = 1;
  }

  return Val_unit;
}
#endif /* _MSC_VER */

#else /* __mc_detect_features__ */

CAMLprim value
mc_detect_cpu_features (__unit ()) {
  return Val_unit;
}

#endif /* __mc_detect_features__ */
