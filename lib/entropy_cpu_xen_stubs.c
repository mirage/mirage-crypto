
#if defined (__i386__) || defined (__x86_64__)
#define __x86__
#endif

#if defined (__x86__)
#include <x86intrin.h>
#include <cpuid.h>
#endif

#include <caml/mlvalues.h>

/* because clang... */
#if !defined(bit_RDSEED)
#define bit_RDSEED 0x00040000
#endif

#if defined (__x86_64__)
#define random_t unsigned long long
#define _rdseed_step _rdseed64_step
#define _rdrand_step _rdrand64_step

#elif defined (__i386__)
#define random_t unsigned int
#define _rdseed_step _rdseed32_step
#define _rdrand_step _rdrand32_step

#endif

enum cpu_rng_t {
  RNG_NONE   = 0,
  RNG_RDRAND = 1,
  RNG_RDSEED = 2,
};

static enum cpu_rng_t __cpu_rng = RNG_NONE;

/* XXX:
 * __attribute__ ((constructor))
 */
static void detect () {
#if defined (__x86__)

  unsigned int sig, eax, ebx, ecx, edx;
  int max = __get_cpuid_max (0, &sig);

  if (max < 1) return;

  if (sig == signature_INTEL_ebx || sig == signature_AMD_ebx) {
    __cpuid (1, eax, ebx, ecx, edx);
    if (ecx & bit_RDRND) __cpu_rng = RNG_RDRAND;
    if (max > 7) {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_RDSEED) __cpu_rng = RNG_RDSEED;
    }
  }

#elif defined (__ARM_ARCH_7A__) && 0
/* XXX:
 * The ideal timing source on ARM are the performance counters, but these are
 * presently masked by Xen.
 * Matches with the bit in caml_cycle_counter.
 */
  /* Disable counter overflow interrupts. */
  __asm__ __volatile__ ("MCR p15, 0, %0, c9, c14, 2" :: "r"(0x8000000f));
  /* Program the PMU control register. */
  __asm__ __volatile__ ("MCR p15, 0, %0, c9, c12, 0" :: "r"(1 | 16));
  /* Enable all counters. */
  __asm__ __volatile__ ("MCR p15, 0, %0, c9, c12, 1" :: "r"(0x8000000f));

#endif
}

CAMLprim value caml_cycle_counter (value unit) {
#if defined (__x86__)
  return Val_long (__rdtsc ());
#elif defined (__ARM_ARCH_7A__) && 0
  unsigned int res;
  __asm__ __volatile__ ("MRC p15, 0, %0, c9, c13, 0": "=r" (res));
  return Val_long (res);
#else
#error ("No known cycle-counting instruction.")
#endif
}

CAMLprim value caml_cpu_random (value unit) {
#if defined (__x86__)
  random_t r = 0;
  if (__cpu_rng == RNG_RDSEED) {
    _rdseed_step (&r);
  } else if (__cpu_rng == RNG_RDRAND) {
    _rdrand_step (&r);
  }
  return Val_long (r); /* Zeroed-out if carry == 0. */
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value caml_cpu_rng_type (value unit) {
  return Val_int (__cpu_rng);
}

CAMLprim value caml_entropy_xen_detect (value unit) {
  detect ();
  return Val_unit;
}
