#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#if defined(__linux)
// on Linux, we use getrandom and loop
#include <sys/random.h>
#include <errno.h>

void raw_getrandom (uint8_t *data, uint32_t len) {
  int r, off = 0;
  while (off < len) {
    r = getrandom(data + off, len - off, 0);
    if (r < 0 && errno == EINTR) continue;
    else if (r < 0) uerror("getrandom", Nothing);
    off += r;
  }
}

#elif (defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__APPLE__))
// on BSD and macOS, loop (in pieces of 256) getentropy
#if defined(__APPLE__)
// on macOS, getentropy is defined in sys/random.h (on BSD in unistd.h)
#include <sys/random.h>
#endif
#include <sys/param.h>

void raw_getrandom (uint8_t *data, uint32_t len) {
  int rlen = 0;
  for (int i = 0; i <= len; i += 256) {
    rlen = MIN(256, len - i);
    if (getentropy(data + i, rlen) < 0) uerror("getentropy", Nothing);
  }
}

#else
#error "Retrieving random data not supported on this platform"
#endif

CAMLprim value mc_getrandom (value ba, value len) {
  raw_getrandom((uint8_t*) Caml_ba_data_val(ba), Int_val(len));
  return Val_unit;
}
