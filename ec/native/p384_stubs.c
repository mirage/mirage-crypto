#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p384_64.h"
#define LIMBS 6
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p384_32.h"
#define LIMBS 12
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 384
#define CURVE_DESCRIPTION fiat_p384

#define FE_LENGTH 48

// Generator point, see https://neuromancer.sk/std/nist/P-384
static uint8_t gb_x[FE_LENGTH] = {0xaa, 0x87, 0xca, 0x22, 0xbe, 0x8b, 0x5, 0x37, 0x8e, 0xb1, 0xc7, 0x1e, 0xf3, 0x20, 0xad, 0x74, 0x6e, 0x1d, 0x3b, 0x62, 0x8b, 0xa7, 0x9b, 0x98, 0x59, 0xf7, 0x41, 0xe0, 0x82, 0x54, 0x2a, 0x38, 0x55, 0x2, 0xf2, 0x5d, 0xbf, 0x55, 0x29, 0x6c, 0x3a, 0x54, 0x5e, 0x38, 0x72, 0x76, 0xa, 0xb7};
static uint8_t gb_y[FE_LENGTH] = {0x36, 0x17, 0xde, 0x4a, 0x96, 0x26, 0x2c, 0x6f, 0x5d, 0x9e, 0x98, 0xbf, 0x92, 0x92, 0xdc, 0x29, 0xf8, 0xf4, 0x1d, 0xbd, 0x28, 0x9a, 0x14, 0x7c, 0xe9, 0xda, 0x31, 0x13, 0xb5, 0xf0, 0xb8, 0xc0, 0xa, 0x60, 0xb1, 0xce, 0x1d, 0x7e, 0x81, 0x9d, 0x7a, 0x43, 0x1d, 0x7c, 0x90, 0xea, 0xe, 0x5f};

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p384_sub(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p384_sub((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_add(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p384_add((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_mul(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p384_mul((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_from_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p384_from_bytes((WORD*)Bytes_val(out), _st_uint8(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_to_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p384_to_bytes(Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_sqr(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p384_square((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_from_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p384_from_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_to_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p384_to_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_nz(value x)
{
    CAMLparam1(x);
    CAMLreturn(Val_bool(fe_nz((const WORD*)String_val(x))));
}

CAMLprim value mc_p384_set_one(value x)
{
    CAMLparam1(x);
        fiat_p384_set_one((WORD*)Bytes_val(x));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_inv(value out, value in)
{
    CAMLparam2(out, in);
    inversion((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p384_point_double(value out, value in)
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

CAMLprim value mc_p384_point_add(value out, value p, value q)
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

CAMLprim value mc_p384_select(value out, value bit, value t, value f)
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

CAMLprim value mc_p384_scalar_mult_base(value out, value s)
{
    CAMLparam2(out, s);
    scalar_mult_base(
        (WORD *) Bytes_val(Field(out, 0)),
        (WORD *) Bytes_val(Field(out, 1)),
        (WORD *) Bytes_val(Field(out, 2)),
        _st_uint8(s),
        caml_string_length(s)
    );
    CAMLreturn(Val_unit);
}

CAMLprim void mc_p384_force_precomputation(void) {
    compute_generator_table();
}
