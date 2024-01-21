#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "np256_64.h"
#define LIMBS 4
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "np256_32.h"
#define LIMBS 8
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 256
#define CURVE_DESCRIPTION fiat_np256

#include "inversion_template.h"

#include <caml/memory.h>

CAMLprim value mc_np256_inv(value out, value in)
{
	CAMLparam2(out, in);
	inversion((WORD*)Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_np256_mul((WORD*)Bytes_val(out), (WORD*)Bytes_val(a), (WORD*)Bytes_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_np256_add((WORD*)Bytes_val(out), (WORD*)Bytes_val(a), (WORD*)Bytes_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_one(value out)
{
	CAMLparam1(out);
	fiat_np256_set_one((WORD*)Bytes_val(out));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np256_from_bytes((WORD*)Bytes_val(out), Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np256_to_bytes(Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_from_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np256_from_montgomery((WORD*)Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_np256_to_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_np256_to_montgomery((WORD*)Bytes_val(out), (WORD*)Bytes_val(in));
	CAMLreturn(Val_unit);
}
