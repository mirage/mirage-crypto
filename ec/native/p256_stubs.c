#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p256_64.h"
#define LIMBS 4
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p256_32.h"
#define LIMBS 8
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 256
#define CURVE_DESCRIPTION fiat_p256

#define FE_LENGTH 32

// Generator point, see https://neuromancer.sk/std/nist/P-256
static uint8_t gb_x[FE_LENGTH] = {0x6b, 0x17, 0xd1, 0xf2, 0xe1, 0x2c, 0x42, 0x47, 0xf8, 0xbc, 0xe6, 0xe5, 0x63, 0xa4, 0x40, 0xf2, 0x77, 0x3, 0x7d, 0x81, 0x2d, 0xeb, 0x33, 0xa0, 0xf4, 0xa1, 0x39, 0x45, 0xd8, 0x98, 0xc2, 0x96};
static uint8_t gb_y[FE_LENGTH] = {0x4f, 0xe3, 0x42, 0xe2, 0xfe, 0x1a, 0x7f, 0x9b, 0x8e, 0xe7, 0xeb, 0x4a, 0x7c, 0xf, 0x9e, 0x16, 0x2b, 0xce, 0x33, 0x57, 0x6b, 0x31, 0x5e, 0xce, 0xcb, 0xb6, 0x40, 0x68, 0x37, 0xbf, 0x51, 0xf5};

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p256_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_sub((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_add((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_mul((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_from_bytes((WORD*)Bytes_val(out), _st_uint8(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_to_bytes(Bytes_val(out), (const WORD*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_square((WORD*)Bytes_val(out), (const WORD*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_from_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_from_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_to_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_to_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_nz(value x)
{
	CAMLparam1(x);
	CAMLreturn(Val_bool(fe_nz((const WORD*)String_val(x))));
}

CAMLprim value mc_p256_set_one(value x)
{
	CAMLparam1(x);
        fiat_p256_set_one((WORD*)Bytes_val(x));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_inv(value out, value in)
{
	CAMLparam2(out, in);
	inversion((WORD*)Bytes_val(out), (const WORD*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_point_double(value out, value in)
{
	CAMLparam2(out, in);
	point_double(
		(WORD*)Bytes_val(Field(out, 0)),
		(WORD*)Bytes_val(Field(out, 1)),
		(WORD*)Bytes_val(Field(out, 2)),
		(const WORD*)String_val(Field(in, 0)),
		(const WORD*)String_val(Field(in, 1)),
		(const WORD*)String_val(Field(in, 2))
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_point_add(value out, value p, value q)
{
	CAMLparam3(out, p, q);
	point_add(
		(WORD*)Bytes_val(Field(out, 0)),
		(WORD*)Bytes_val(Field(out, 1)),
		(WORD*)Bytes_val(Field(out, 2)),
		(const WORD*)String_val(Field(p, 0)),
		(const WORD*)String_val(Field(p, 1)),
		(const WORD*)String_val(Field(p, 2)),
		0,
		(const WORD*)String_val(Field(q, 0)),
		(const WORD*)String_val(Field(q, 1)),
		(const WORD*)String_val(Field(q, 2))
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_select(value out, value bit, value t, value f)
{
	CAMLparam4(out, bit, t, f);
	fe_cmovznz(
		(WORD*)Bytes_val(out),
		Bool_val(bit),
		(const WORD*)String_val(f),
		(const WORD*)String_val(t)
	);
	CAMLreturn(Val_unit);
}


CAMLprim value mc_p256_scalar_mult_base(value out, value s)
{
    CAMLparam2(out, s);
    scalar_mult_base(
		(WORD *) Bytes_val(Field(out, 0)),
		(WORD *) Bytes_val(Field(out, 1)),
		(WORD *) Bytes_val(Field(out, 2)),
        (uint8_t *) String_val(s),
        caml_string_length(s)
    );
    CAMLreturn(Val_unit);
}

CAMLprim void mc_p256_force_precomputation(void) {
    compute_generator_table();
}
