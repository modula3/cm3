/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

typedef unsigned int uint, uint32; /* verified below via UINT_MAX */

#ifdef _WIN32
#define M3_EXTRACT_INSERT_LINKAGE
#else
#define M3_EXTRACT_INSERT_LINKAGE static /* for testing */
#endif

#ifdef _MSC_VER
#pragma warning(disable:4255) /* () changed to (void) */
#pragma warning(disable:4505) /* unused static function removed */
#pragma warning(disable:4711) /* automatic inlining */
#undef _DLL
#ifndef _MT
#define _MT
#endif
typedef __int64 int64;
typedef unsigned __int64 uint64;
#define I64 "I64"
#else
typedef long long int64;
typedef unsigned long long uint64;
#define I64 "ll"
#endif

#include <limits.h>
#include <string.h>
#include <assert.h>
#include <stddef.h>

typedef int BOOL;

#ifdef __cplusplus
extern "C"
{           
#endif

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

#if (UINT_MAX <= 0xFFFF) || (UINT_MAX != 0xFFFFFFFF) || (UINT_MAX != 0xFFFFFFFFUL)
#error uint is not 32bits
#endif

/* There are problems passing int64 in K&R form! */
#if 1 /* defined(__STDC__) || defined(__cplusplus) || defined(_MSC_VER) */
#define ANSI(x) x
#define KR(x)
#else
#define ANSI(x)
#define KR(x) x
#endif

#ifdef _WIN32

/* return positive form of a negative value, avoiding overflow */
/* T should be an unsigned type */
#define M3_POS(T, a) (((T)-((a) + 1)) + 1)

int64 __stdcall m3_div64(int64 b, int64 a)
{
  typedef  int64 ST; /* signed type */
  typedef uint64 UT; /* unsigned type */
  int aneg = (a < 0);
  int bneg = (b < 0);
  if (aneg == bneg || a == 0 || b == 0)
    return (a / b);
  else
  {
    /* round negative result down by rounding positive result up
       unsigned math is much better defined, see gcc -Wstrict-overflow=4 */
    UT ua = (aneg ? M3_POS(UT, a) : (UT)a);
    UT ub = (bneg ? M3_POS(UT, b) : (UT)b);
    return -(ST)((ua + ub - 1) / ub);
  }
}

int64 __stdcall m3_mod64(int64 b, int64 a)
{
  typedef  int64 ST; /* signed type */
  typedef uint64 UT; /* unsigned type */
  int aneg = (a < 0);
  int bneg = (b < 0);
  if (aneg == bneg || a == 0 || b == 0)
    return (a % b);
  else
  {
    UT ua = (aneg ? M3_POS(UT, a) : (UT)a);
    UT ub = (bneg ? M3_POS(UT, b) : (UT)b);
    a = (ST)(ub - 1 - (ua + ub - 1) % ub);
    return (bneg ? -a : a);
  }
}

#endif

#define SET_GRAIN (sizeof (size_t) * 8)

void __stdcall set_union
    ANSI((                 size_t n_bits, size_t* c, size_t* b, size_t* a))
      KR((n_bits, c, b, a) size_t n_bits; size_t* c; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] | c[i];
  }
}

void __stdcall set_intersection
    ANSI((                 size_t n_bits, size_t* c, size_t* b, size_t* a))
      KR((n_bits, c, b, a) size_t n_bits; size_t* c; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & c[i];
  }
}

void __stdcall set_difference
    ANSI((                 size_t n_bits, size_t* c, size_t* b, size_t* a))
      KR((n_bits, c, b, a) size_t n_bits; size_t* c; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & (~ c[i]);
  }
}

void __stdcall set_sym_difference
    ANSI((                 size_t n_bits, size_t* c, size_t* b, size_t* a))
      KR((n_bits, c, b, a) size_t n_bits; size_t* c; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] ^ c[i];
  }
}

size_t __stdcall set_ge
    ANSI((              size_t n_bits, size_t* b, size_t* a))
      KR((n_bits, b, a) size_t n_bits; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
  }
  return 1;
}

size_t __stdcall set_gt
    ANSI((              size_t n_bits, size_t* b, size_t* a))
      KR((n_bits, b, a) size_t n_bits; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  register size_t eq = 0;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
    eq |=  (a[i] ^ b[i]);
  }
  return (eq != 0);
}

size_t __stdcall set_le
    ANSI((              size_t n_bits, size_t* b, size_t* a))
      KR((n_bits, b, a) size_t n_bits; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
  }
  return 1;
}

size_t __stdcall set_lt
    ANSI((              size_t n_bits, size_t* b, size_t* a))
      KR((n_bits, b, a) size_t n_bits; size_t* b; size_t* a;)
{
  register size_t n_words = n_bits / SET_GRAIN;
  register size_t i;
  register size_t eq = 0;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
    eq |= (a[i] ^ b[i]);
  }
  return (eq != 0);
}

#define _LOWBITS(a)  ((a) ? ((~(size_t)0) >> ((sizeof(size_t) * 8) - (a))) : 0)
#define _HIGHBITS(a) (((a) < (sizeof(size_t) * 8)) ? ((~(size_t)0) << (a)) : 0)

#ifdef _WIN32

/* _lowbits[i] = bits{(i-1)..0} for 32-bit integer masks */
#ifdef __cplusplus
extern
#endif
const uint _lowbits [33] = {
  0x0,
  0x1, 0x3, 0x7, 0xf,
  0x1f, 0x3f, 0x7f, 0xff,
  0x1ff, 0x3ff, 0x7ff, 0xfff,
  0x1fff, 0x3fff, 0x7fff, 0xffff,
  0x1ffff, 0x3ffff, 0x7ffff, 0xfffff,
  0x1fffff, 0x3fffff, 0x7fffff, 0xffffff,
  0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff,
  0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff };

/* _highbits[i] = bits{31..i} for 32-bit integer masks */
#ifdef __cplusplus
extern
#endif
const uint _highbits [33] = {
  0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8,
  0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80,
  0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800,
  0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000,
  0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000,
  0xfff00000, 0xffe00000, 0xffc00000, 0xff800000,
  0xff000000, 0xfe000000, 0xfc000000, 0xf8000000,
  0xf0000000, 0xe0000000, 0xc0000000, 0x80000000,
  0x0 };

#endif

#define HIGH_BITS(a) ((~(size_t)0) << (a))
#define LOW_BITS(a)  ((~(size_t)0) >> (SET_GRAIN - (a) - 1))

static void __stdcall set_range
    ANSI((       size_t b, size_t a, size_t* s))
    KR((b, a, s) size_t b; size_t a; size_t* s;)
{
  if (b < a) {
      /* no bits to set */
  } else {
      size_t a_word = a / SET_GRAIN;
      size_t b_word = b / SET_GRAIN;
      size_t i;
      size_t high_bits = HIGH_BITS(a % SET_GRAIN);
      size_t low_bits = LOW_BITS(b % SET_GRAIN);

      if (a_word == b_word) {
          s [a_word] |= (high_bits & low_bits);
      } else {
          s [a_word] |= high_bits;
          for (i = a_word + 1; i < b_word; ++i)
            s[i] = ~0UL;
          s [b_word] |= low_bits;
      }
    }
}

#ifdef _WIN32

/* Several functions are "missing" here because we
 * call the C compiler helper functions directly.
 * e.g. multiply, unsigned div/mod, shift left/right.
 * Also some operations are generated inline, e.g. add, subtract, compare, and, or, xor, not, neg, abs.
 */

uint64 _rotl64(uint64 value, int shift);
uint64 _rotr64(uint64 value, int shift);
#pragma intrinsic(_rotl64)
#pragma intrinsic(_rotr64)
uint64 __stdcall  m3_rotate_left64(uint64 a, uint b)  { return _rotl64(a, (int)b); }
uint64 __stdcall m3_rotate_right64(uint64 a, uint b)  { return _rotr64(a, (int)b); }

uint64 __stdcall m3_rotate64(uint64 a, int b)
{
    b &= 63;
    if (b > 0)
        a = _rotl64(a, b);
    else if (b < 0)
        a = _rotr64(a, -b);
    return a;
}

#endif /* WIN32 */

/*
 PROCEDURE Extract (x: T; i, n: CARDINAL): T;
(* Take n bits from x, with bit i as the least significant bit, and return them
   as the least significant n bits of a word whose other bits are 0. A checked
   runtime error if n + i > Word.Size. *)

The extract call in the backend has a boolean sign_extend parameter as well.
This is used for signed integer division by a power of two.

PROCEDURE Insert (x, y: T; i, n: CARDINAL): T;
(* Return x with n bits replaced, with bit i as the least significant bit, by
   the least significant n bits of y. The other bits of x are unchanged. A
   checked runtime error if n + i > Word.Size. *)
*/

#define M3_EXTRACT_INSERT(extract, extract_and_sign_extend, insert, T)  \
                                                \
M3_EXTRACT_INSERT_LINKAGE                       \
T __stdcall extract(T x, uint i, uint n)        \
{                                               \
    assert((n + i) <= (sizeof(T) * 8));         \
    x >>= i;                                    \
    x &= ~((~(T)0) << n);                       \
    return x;                                   \
}                                               \
                                                \
M3_EXTRACT_INSERT_LINKAGE                       \
T __stdcall extract_and_sign_extend(T x, uint i, uint n) \
{                                               \
    assert((n + i) <= (sizeof(T) * 8));         \
    x >>= i;                                    \
    x &= ~((~(T)0) << n);                       \
    if (x & (((T)1) << (n - 1)))                \
        x |= ((~(T)0) << n);                    \
    return x;                                   \
}                                               \
                                                \
M3_EXTRACT_INSERT_LINKAGE                       \
T __stdcall insert(T x, T y, uint i, uint n)    \
{                                               \
    T mask = ((~((~(T)0) << n)) << i);          \
    assert((n + i) <= (sizeof(T) * 8));         \
    return (x & ~mask) | ((y << i) & mask);     \
}                                               \

M3_EXTRACT_INSERT(m3_extract64, m3_extract_and_sign_extend64, m3_insert64, uint64)

#ifdef __cplusplus
} /* extern "C" */
#endif
