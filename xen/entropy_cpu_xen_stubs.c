
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

static int __has_rdseed = 0;
static int __has_rdrand = 0;

/* XXX:
__attribute__ ((constructor))
 */
static void init () {
#if defined (__x86__)

  unsigned int sig, eax, ebx, ecx, edx;
  int max = __get_cpuid_max (0, &sig);

  if (max < 1) return;

  if (sig == signature_INTEL_ebx || sig == signature_AMD_ebx) {
    __cpuid (1, eax, ebx, ecx, edx);
    __has_rdrand = (ecx & bit_RDRND) != 0;
    if (max > 7) {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      __has_rdseed = (ebx & bit_RDSEED) != 0;
    }
  }
#endif
}

CAMLprim value caml_cycle_counter (value unit) {
#if defined (__x86__)
  return Val_long (__rdtsc ());
#else
  /* ARM: Plug an equivalent to RDTSC[P] here. */
#error ("No known cycle-counting instruction.")
#endif
}

CAMLprim value caml_cpu_random (value unit) {
#if defined (__x86__)
  random_t r = 0;
  if (__has_rdseed) {
    _rdseed_step (&r);
  } else if (__has_rdrand) {
    _rdrand_step (&r);
  }
  return Val_long (r); /* Zeroed-out if carry == 0. */
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value caml_has_rdrand (value unit) {
  return Val_bool (__has_rdrand);
}

CAMLprim value caml_has_rdseed (value unit) {
  return Val_bool (__has_rdseed);
}

CAMLprim value caml_entropy_xen_init (value unit) {
  init ();
  return Val_unit;
}
