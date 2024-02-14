#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p521_64.h"
#define LIMBS 9
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p521_32.h"
#define LIMBS 17
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 521
#define CURVE_DESCRIPTION fiat_p521

#define FE_LENGTH 66

// Generator point, see https://neuromancer.sk/std/nist/P-521
static uint8_t gb_x[FE_LENGTH] = {0x0, 0xc6, 0x85, 0x8e, 0x6, 0xb7, 0x4, 0x4, 0xe9, 0xcd, 0x9e, 0x3e, 0xcb, 0x66, 0x23, 0x95, 0xb4, 0x42, 0x9c, 0x64, 0x81, 0x39, 0x5, 0x3f, 0xb5, 0x21, 0xf8, 0x28, 0xaf, 0x60, 0x6b, 0x4d, 0x3d, 0xba, 0xa1, 0x4b, 0x5e, 0x77, 0xef, 0xe7, 0x59, 0x28, 0xfe, 0x1d, 0xc1, 0x27, 0xa2, 0xff, 0xa8, 0xde, 0x33, 0x48, 0xb3, 0xc1, 0x85, 0x6a, 0x42, 0x9b, 0xf9, 0x7e, 0x7e, 0x31, 0xc2, 0xe5, 0xbd, 0x66};
static uint8_t gb_y[FE_LENGTH] = {0x1, 0x18, 0x39, 0x29, 0x6a, 0x78, 0x9a, 0x3b, 0xc0, 0x4, 0x5c, 0x8a, 0x5f, 0xb4, 0x2c, 0x7d, 0x1b, 0xd9, 0x98, 0xf5, 0x44, 0x49, 0x57, 0x9b, 0x44, 0x68, 0x17, 0xaf, 0xbd, 0x17, 0x27, 0x3e, 0x66, 0x2c, 0x97, 0xee, 0x72, 0x99, 0x5e, 0xf4, 0x26, 0x40, 0xc5, 0x50, 0xb9, 0x1, 0x3f, 0xad, 0x7, 0x61, 0x35, 0x3c, 0x70, 0x86, 0xa2, 0x72, 0xc2, 0x40, 0x88, 0xbe, 0x94, 0x76, 0x9f, 0xd1, 0x66, 0x50};

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p521_sub(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p521_sub((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_add(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p521_add((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_mul(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p521_mul((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_from_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p521_from_bytes((WORD*)Bytes_val(out), _st_uint8(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_to_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p521_to_bytes(Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_sqr(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p521_square((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_from_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p521_from_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_to_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p521_to_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_nz(value x)
{
    CAMLparam1(x);
    CAMLreturn(Val_bool(fe_nz((const WORD*)String_val(x))));
}

CAMLprim value mc_p521_set_one(value x)
{
    CAMLparam1(x);
        fiat_p521_set_one((WORD*)Bytes_val(x));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_inv(value out, value in)
{
    CAMLparam2(out, in);
    inversion((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p521_point_double(value out, value in)
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

CAMLprim value mc_p521_point_add(value out, value p, value q)
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

CAMLprim value mc_p521_select(value out, value bit, value t, value f)
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

CAMLprim value mc_p521_scalar_mult_base(value out, value s)
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

CAMLprim void mc_p521_force_precomputation(void) {
    compute_generator_table();
}
