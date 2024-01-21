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

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p256_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_sub((WORD*)Bytes_val(out), (WORD*)Bytes_val(a), (WORD*)Bytes_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_add((WORD*)Bytes_val(out), (WORD*)Bytes_val(a), (WORD*)Bytes_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_p256_mul((WORD*)Bytes_val(out), (WORD*)Bytes_val(a), (WORD*)Bytes_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_from_bytes((WORD*)Bytes_val(out), Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_to_bytes(Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_p256_square((WORD*)Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_from_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = (WORD*)Bytes_val(x);
	fiat_p256_from_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_to_montgomery(value x)
{
	CAMLparam1(x);
	WORD *l = (WORD*)Bytes_val(x);
	fiat_p256_to_montgomery(l, l);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_nz(value x)
{
	CAMLparam1(x);
	CAMLreturn(Val_bool(fe_nz((WORD*)Bytes_val(x))));
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
	inversion((WORD*)Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_point_double(value out, value in)
{
	CAMLparam2(out, in);
	point_double(
		(WORD*)Bytes_val(Field(out, 0)),
		(WORD*)Bytes_val(Field(out, 1)),
		(WORD*)Bytes_val(Field(out, 2)),
		(WORD*)Bytes_val(Field(in, 0)),
		(WORD*)Bytes_val(Field(in, 1)),
		(WORD*)Bytes_val(Field(in, 2))
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
		(WORD*)Bytes_val(Field(p, 0)),
		(WORD*)Bytes_val(Field(p, 1)),
		(WORD*)Bytes_val(Field(p, 2)),
		0,
		(WORD*)Bytes_val(Field(q, 0)),
		(WORD*)Bytes_val(Field(q, 1)),
		(WORD*)Bytes_val(Field(q, 2))
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_p256_select(value out, value bit, value t, value f)
{
	CAMLparam4(out, bit, t, f);
	fe_cmovznz(
		(WORD*)Bytes_val(out),
		Bool_val(bit),
		(WORD*)Bytes_val(f),
		(WORD*)Bytes_val(t)
	);
	CAMLreturn(Val_unit);
}
