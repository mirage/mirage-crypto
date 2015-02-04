#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <stdio.h>

#define __unlikely(x) __builtin_expect((x), 0)

/* XXX
 * Everything here is x86 only.
 */

typedef struct cpuid {
  unsigned int eax;
  unsigned int ebx;
  unsigned int ecx;
  unsigned int edx;
} cpuid_t;

static inline void cpuid (cpuid_t *id, unsigned int leaf, unsigned int subleaf) {
  /* XXX
   * __i386__ needs saving %ebx with -fPIC.
   */
  asm volatile ("cpuid"
      : "=a" (id->eax), "=b" (id->ebx), "=c" (id->ecx), "=d" (id->edx)
      : "a" (leaf), "c" (subleaf)
    );
}

static inline int has_rdrand () {
  static int r = -1;
  if (r == -1) {
    cpuid_t id;
    cpuid (&id, 1, 0);
    r = (id.ecx & 0x40000000) == 0x40000000;
  }
  return r;
}

static inline int has_rdseed () {
  static int r = -1;
  if (r == -1) {
    cpuid_t id;
    cpuid (&id, 7, 0);
    r = (id.ebx & 0x40000) == 0x40000;
  }
  return r;
}


/* XXX
 * arch switch the typedef - rdXXX work on 32 too.
 */
typedef uint64_t rand_t ;

static inline void rdtscp (unsigned int *hi, unsigned int *lo) {
  asm volatile ("rdtscp" : "=a" (*lo), "=d" (*hi));
}

static inline int rdrand (rand_t *r) {
  unsigned char e;
  asm volatile ("rdrand %0; setc %1" : "=r" (*r), "=qm" (e));
  return e;
}

static inline int rdseed (rand_t *r) {
  unsigned char e;
  asm volatile ("rdseed %0; setc %1" : "=r" (*r), "=qm" (e));
  return e;
}


static const char no_instr_id []   = "entropy instruction unavailable";
static const char no_entropy_id [] = "entropy pool empty";

CAMLprim value caml_rdtscp () {
  unsigned int hi, lo;
  rdtscp (&hi, &lo);
  return Val_long(((intnat)hi << 32) | (intnat)lo);
}

CAMLprim value caml_rdrand () {
  rand_t r;
  if (__unlikely(!has_rdrand ()))
    caml_raise_with_string (* caml_named_value (no_instr_id), "rdrand");
  unsigned char e = rdrand(&r);
  if (__unlikely(!e))
    caml_raise_constant (*caml_named_value (no_entropy_id));
  return Val_long(r);
}

CAMLprim value caml_rdseed () {
  rand_t r;
  if (__unlikely(!has_rdseed ()))
    caml_raise_with_string (* caml_named_value (no_instr_id), "rdseed");
  unsigned char e = rdseed(&r);
  if (__unlikely(!e))
    caml_raise_constant (*caml_named_value (no_entropy_id));
  return Val_long(r);
}
