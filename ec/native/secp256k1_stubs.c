#include "mirage_crypto.h"
#include "secp256k1_kiila.h"
#include <caml/memory.h>

CAMLprim value mc_secp256k1_sub(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_secp256k1_sub((limb_t*)Bytes_val(out), (const limb_t*)String_val(a), (const limb_t*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_secp256k1_add((limb_t*)Bytes_val(out), (const limb_t*)String_val(a), (const limb_t*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_secp256k1_mul((limb_t*)Bytes_val(out), (const limb_t*)String_val(a), (const limb_t*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_from_bytes((limb_t*)Bytes_val(out), _st_uint8(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_to_bytes(Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_sqr(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_square((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_from_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_from_montgomery((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_to_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_to_montgomery((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_nz(value x)
{
	CAMLparam1(x);
	limb_t ret;
	fiat_secp256k1_nonzero(&ret, (const limb_t*)String_val(x));
	CAMLreturn(Val_bool(ret));
}

CAMLprim value mc_secp256k1_set_one(value x)
{
	CAMLparam1(x);
    fiat_secp256k1_set_one((limb_t*)Bytes_val(x));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_inv(value out, value in)
{
	CAMLparam2(out, in);
	fiat_secp256k1_inv((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_select(value out, value bit, value t, value f)
{
	CAMLparam4(out, bit, t, f);
	fiat_secp256k1_selectznz(
		(limb_t*)Bytes_val(out),
		!!Bool_val(bit),
		(const limb_t*)String_val(f),
		(const limb_t*)String_val(t)
	);
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_inv(value out, value in)
{
	CAMLparam2(out, in);
	fiat_nsecp256k1_inv((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_mul(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_nsecp256k1_mul((limb_t*)Bytes_val(out), (const limb_t*)String_val(a), (const limb_t*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_add(value out, value a, value b)
{
	CAMLparam3(out, a, b);
	fiat_nsecp256k1_add((limb_t*)Bytes_val(out), (const limb_t*)String_val(a), (const limb_t*)String_val(b));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_one(value out)
{
	CAMLparam1(out);
	fiat_nsecp256k1_set_one((limb_t*)Bytes_val(out));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_from_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_nsecp256k1_from_bytes((limb_t*)Bytes_val(out), _st_uint8(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_to_bytes(value out, value in)
{
	CAMLparam2(out, in);
	fiat_nsecp256k1_to_bytes(Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_from_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_nsecp256k1_from_montgomery((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

CAMLprim value mc_nsecp256k1_to_montgomery(value out, value in)
{
	CAMLparam2(out, in);
	fiat_nsecp256k1_to_montgomery((limb_t*)Bytes_val(out), (const limb_t*)String_val(in));
	CAMLreturn(Val_unit);
}

#define PT(v) ((const pt_aff_t*) (String_val(v)))
#define PT_OUT(v) ((pt_aff_t*) (Bytes_val(v)))

CAMLprim value mc_secp256k1_scalar_mult_base(value out, value s)
{
	CAMLparam2(out, s);
	fixed_smul_cmb(PT_OUT(out), _st_uint8(s));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_scalar_mult(value out, value s, value p)
{
    CAMLparam3(out, s, p);
	var_smul_rwnaf(PT_OUT(out), _st_uint8(s), PT(p));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_secp256k1_scalar_mult_add(value out, value a, value b, value p)
{
	CAMLparam4(out, a, b, p);
    var_smul_wnaf_two(PT_OUT(out), _st_uint8(a), _st_uint8(b), PT(p));
    CAMLreturn(Val_unit);
}
