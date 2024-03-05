#ifndef _MSC_VER
# include <unistd.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#if defined(__linux) || defined(__GNU__)
# include <errno.h>
// on Linux and GNU/Hurd, we use getrandom and loop

# if __GLIBC__ && __GLIBC__ <= 2 && __GLIBC_MINOR__ < 25
# include <sys/syscall.h>
# define getrandom(buf, len, flags) syscall(SYS_getrandom, (buf), (len), (flags))
# else
# include <sys/random.h>
# define getrandom(buf, len, flags) getrandom((buf), (len), (flags))
# endif

void raw_getrandom (uint8_t *data, uint32_t len) {
  size_t off = 0;
  ssize_t r = 0;
  while (off < len) {
    r = getrandom(data + off, len - off, 0);
    if (r == -1) {
      if (errno == EINTR) continue;
      else uerror("getrandom", Nothing);
    }
    off += (size_t)r;
  }
}
#elif (defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || defined(__DragonFly__)  || defined(__OpenBSD__) || defined(__APPLE__)) || defined(__NetBSD__)
// on BSD and macOS, loop (in pieces of 256) getentropy
#if defined(__APPLE__)
// on macOS, getentropy is defined in sys/random.h (on BSD in unistd.h)
#include <sys/random.h>
#endif
#include <sys/param.h>

void raw_getrandom (uint8_t *data, uint32_t len) {
  size_t rlen = 0;
  for (uint32_t i = 0; i <= len; i += 256) {
    rlen = MIN(256, len - i);
    if (getentropy(data + i, rlen) == -1) uerror("getentropy", Nothing);
  }
}
#elif (defined(_WIN32))
/* There is a choice between using RtlGenRandom and BCryptGenRandom
 * here, and Microsoft does not make the choice obvious. It appears
 * that RtlGenRandom is best used when older Windows compatibility
 * is of concern, but requires some gymnastics around binding it
 * with the right calling convention.
 *
 * Therefore (https://github.com/mirage/mirage-crypto/pull/39) we
 * have decided to go with the more modern Windows API with bcrypt,
 * and make Windows 10 our minimum supported version of mirage-crypto.
 */
#include <Windows.h>
#include <ntstatus.h>
#include <bcrypt.h>

void raw_getrandom(uint8_t *data, uint32_t len) {
   NTSTATUS Status;
   Status = BCryptGenRandom(NULL, data, len, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
   if (Status != STATUS_SUCCESS)
     uerror("BCryptGenRandom", Nothing);
}

#else
#error "Retrieving random data not supported on this platform"
#endif

CAMLprim value mc_getrandom (value buf, value len) {
  raw_getrandom(Bytes_val(buf), Int_val(len));
  return Val_unit;
}
