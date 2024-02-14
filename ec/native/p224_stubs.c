#include "mirage_crypto.h"

#ifdef ARCH_64BIT
#include "p224_64.h"
#define LIMBS 4
#define WORD uint64_t
#define WORDSIZE 64
#else
#include "p224_32.h"
#define LIMBS 7
#define WORD uint32_t
#define WORDSIZE 32
#endif

#define LEN_PRIME 224
#define CURVE_DESCRIPTION fiat_p224

#define FE_LENGTH 28

// Generator point, see https://neuromancer.sk/std/nist/P-224
static uint8_t gb_x[FE_LENGTH] = {0xb7, 0xe, 0xc, 0xbd, 0x6b, 0xb4, 0xbf, 0x7f, 0x32, 0x13, 0x90, 0xb9, 0x4a, 0x3, 0xc1, 0xd3, 0x56, 0xc2, 0x11, 0x22, 0x34, 0x32, 0x80, 0xd6, 0x11, 0x5c, 0x1d, 0x21};
static uint8_t gb_y[FE_LENGTH] = {0xbd, 0x37, 0x63, 0x88, 0xb5, 0xf7, 0x23, 0xfb, 0x4c, 0x22, 0xdf, 0xe6, 0xcd, 0x43, 0x75, 0xa0, 0x5a, 0x7, 0x47, 0x64, 0x44, 0xd5, 0x81, 0x99, 0x85, 0x0, 0x7e, 0x34};

#include "inversion_template.h"
#include "point_operations.h"

#include <caml/memory.h>

CAMLprim value mc_p224_sub(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p224_sub((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_add(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p224_add((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_mul(value out, value a, value b)
{
    CAMLparam3(out, a, b);
    fiat_p224_mul((WORD*)Bytes_val(out), (const WORD*)String_val(a), (const WORD*)String_val(b));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_from_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p224_from_bytes((WORD*)Bytes_val(out), _st_uint8(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_to_bytes(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p224_to_bytes(Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_sqr(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p224_square((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_from_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p224_from_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_to_montgomery(value out, value in)
{
    CAMLparam2(out, in);
    fiat_p224_to_montgomery((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_nz(value x)
{
    CAMLparam1(x);
    CAMLreturn(Val_bool(fe_nz((const WORD*)String_val(x))));
}

CAMLprim value mc_p224_set_one(value x)
{
    CAMLparam1(x);
        fiat_p224_set_one((WORD*)Bytes_val(x));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_inv(value out, value in)
{
    CAMLparam2(out, in);
    inversion((WORD*)Bytes_val(out), (const WORD*)String_val(in));
    CAMLreturn(Val_unit);
}

CAMLprim value mc_p224_point_double(value out, value in)
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

CAMLprim value mc_p224_point_add(value out, value p, value q)
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

CAMLprim value mc_p224_select(value out, value bit, value t, value f)
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

CAMLprim value mc_p224_scalar_mult_base(value out, value s)
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

CAMLprim void mc_p224_force_precomputation(void) {
    compute_generator_table();
}
