#include <stdint.h>

#if ( defined (__i386__) || defined (__x86_64__) )
#include <x86intrin.h>
#endif

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>

#define __unlikely(x) __builtin_expect((x), 0)

static const char no_instr_id   [] = "instruction not available";
static const char no_entropy_id [] = "entropy pool empty";

#define raise_not_available() \
  (caml_raise_constant (*caml_named_value (no_instr_id)))

#define raise_no_entropy() \
  (caml_raise_constant (*caml_named_value (no_entropy_id)))


#if defined(__x86_64__)

typedef uint64_t rand_t; /* XXX RDxxx work on __x386__ too. */

typedef struct cpuid {
  unsigned int eax;
  unsigned int ebx;
  unsigned int ecx;
  unsigned int edx;
} cpuid_t;

static inline void cpuid (cpuid_t *id, unsigned int leaf, unsigned int subleaf) {
  /* XXX __i386__ needs saving %ebx with -fPIC.  */
  asm volatile ("cpuid"
      : "=a" (id->eax), "=b" (id->ebx), "=c" (id->ecx), "=d" (id->edx)
      : "a" (leaf), "c" (subleaf)
    );
}

/* XXX Assumes Intel. */
static inline int has_rdrand () {
  static int r = -1;
  if (r == -1) {
    cpuid_t id;
    cpuid (&id, 1, 0);
    r = (id.ecx & 0x40000000) == 0x40000000;
  }
  return r;
}

/* XXX Assumes Intel. */
static inline int has_rdseed () {
  static int r = -1;
  if (r == -1) {
    cpuid_t id;
    cpuid (&id, 7, 0);
    r = (id.ebx & 0x40000) == 0x40000;
  }
  return r;
}
#endif /* __x86_64__ */

static int __has_rdseed = 0;
static int __has_rdrand = 0;

CAMLprim value caml_cycle_counter () {
#if defined(__i386__) || defined(__x86_64__)
  return Val_long (__rdtsc ());
#else
  /* ARM: Plug an equivalent to RDTSC[P] here. */
#error ("No known cycle-counting instruction.")
#endif
}

#if defined (__x86_64__)
#define random_t unsigned long long
#define _rdseed_step _rdseed64_step
#define _rdrand_step _rdrand64_step

#elif defined (__i386__)
#define random_t unsigned int
#define _rdseed_step _rdseed32_step
#define _rdrand_step _rdrand32_step

#endif

CAMLprim value caml_cpu_random () {
#if defined (__i386__) || defined (__x86_64__)
  random_t r = 0;
  if (__has_rdseed) {
    _rdseed_step (&r);
  } else if (__has_rdrand) {
    _rdrand_step (&r);
  }
  return Val_long (r);
#else
  /* ARM: CPU-assisted randomness here. */
  return Val_long (0);
#endif
}

CAMLprim value caml_has_rdrand () {
  return Val_bool (__has_rdrand);
}

CAMLprim value caml_has_rdseed () {
  return Val_bool (__has_rdseed);
}
