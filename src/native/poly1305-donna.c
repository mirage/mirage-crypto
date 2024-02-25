// from https://github.com/floodyberry/poly1305-donna.git

#include "mirage_crypto.h"

typedef struct poly1305_context {
        size_t aligner;
        unsigned char opaque[136];
} poly1305_context;

/* 64-bit Windows sets ARCH_64BIT but poly1305-donna-64 requires 128-bit integers
 * that are not supported by the Microsoft compiler. Drop down to 32-bit for MSVC.
 */
#if defined(ARCH_64BIT) && !defined(_MSC_VER)
#include "poly1305-donna-64.h"
#else
#include "poly1305-donna-32.h"
#endif

static void
poly1305_update(poly1305_context *ctx, const unsigned char *m, size_t bytes) {
	poly1305_state_internal_t *st = (poly1305_state_internal_t *)ctx;
	size_t i;

	/* handle leftover */
	if (st->leftover) {
		size_t want = (poly1305_block_size - st->leftover);
		if (want > bytes)
			want = bytes;
		for (i = 0; i < want; i++)
			st->buffer[st->leftover + i] = m[i];
		bytes -= want;
		m += want;
		st->leftover += want;
		if (st->leftover < poly1305_block_size)
			return;
		poly1305_blocks(st, st->buffer, poly1305_block_size);
		st->leftover = 0;
	}

	/* process full blocks */
	if (bytes >= poly1305_block_size) {
		size_t want = (bytes & ~(poly1305_block_size - 1));
		poly1305_blocks(st, m, want);
		m += want;
		bytes -= want;
	}

	/* store leftover */
	if (bytes) {
		for (i = 0; i < bytes; i++)
			st->buffer[st->leftover + i] = m[i];
		st->leftover += bytes;
	}
}

//stubs for OCaml
CAMLprim value mc_poly1305_init (value ctx, value key) {
  poly1305_init ((poly1305_context *) Bytes_val(ctx), _st_uint8(key));
  return Val_unit;
}

CAMLprim value mc_poly1305_update (value ctx, value buf, value len) {
  poly1305_update ((poly1305_context *) Bytes_val(ctx), _st_uint8(buf), Int_val(len));
  return Val_unit;
}

CAMLprim value mc_poly1305_finalize (value ctx, value mac) {
  poly1305_finish ((poly1305_context *) Bytes_val(ctx), Bytes_val(mac));
  return Val_unit;
}

CAMLprim value mc_poly1305_ctx_size (__unit ()) {
  return Val_int(sizeof(poly1305_context));
}

CAMLprim value mc_poly1305_mac_size (__unit ()) {
  return Val_int(16);
}
