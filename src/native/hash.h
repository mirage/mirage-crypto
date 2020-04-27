/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef HASH_H
#define HASH_H

#include "mirage_crypto.h"

static inline void array_swap32(uint32_t *d, uint32_t *s, uint32_t nb)
{
  while (nb--) *d++ = bswap32(*s++);
}

static inline void array_swap64(uint64_t *d, uint64_t *s, uint32_t nb)
{
  while (nb--) *d++ = bswap64(*s++);
}

static inline void array_copy32(uint32_t *d, uint32_t *s, uint32_t nb)
{
  while (nb--) *d++ = *s++;
}

static inline void array_copy64(uint64_t *d, uint64_t *s, uint32_t nb)
{
  while (nb--) *d++ = *s++;
}

#if __BYTE_ORDER == __LITTLE_ENDIAN

#define htole32_array(d, s, l) array_copy32(d, s, l)
#define le32toh_array(d, s, l) array_copy32(d, s, l)
#define htobe32_array(d, s, l) array_swap32(d, s, l)
#define be32toh_array(d, s, l) array_swap32(d, s, l)

#define htole64_array(d, s, l) array_copy64(d, s, l)
#define le64toh_array(d, s, l) array_copy64(d, s, l)
#define htobe64_array(d, s, l) array_swap64(d, s, l)
#define be64toh_array(d, s, l) array_swap64(d, s, l)

#else /* BYTE_ORDER != __LITTLE_ENDIAN */

#define htole32_array(d, s, l) array_swap32(d, s, l)
#define le32toh_array(d, s, l) array_swap32(d, s, l)
#define htobe32_array(d, s, l) array_copy32(d, s, l)
#define be32toh_array(d, s, l) array_copy32(d, s, l)

#define htole64_array(d, s, l) array_swap64(d, s, l)
#define le64toh_array(d, s, l) array_swap64(d, s, l)
#define htobe64_array(d, s, l) array_copy64(d, s, l)
#define be64toh_array(d, s, l) array_copy64(d, s, l)

#endif /* BYTE_ORDER == __LITTLE_ENDIAN */

static inline uint32_t rol32(uint32_t word, uint32_t shift)
{
  return (word << shift) | (word >> (32 - shift));
}

static inline uint32_t ror32(uint32_t word, uint32_t shift)
{
  return (word >> shift) | (word << (32 - shift));
}

static inline uint64_t rol64(uint64_t word, uint32_t shift)
{
  return (word << shift) | (word >> (64 - shift));
}

static inline uint64_t ror64(uint64_t word, uint32_t shift)
{
  return (word >> shift) | (word << (64 - shift));
}

#endif /* !HASH_H */
