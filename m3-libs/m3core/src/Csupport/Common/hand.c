/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

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
#else
typedef long long int64;
typedef unsigned long long uint64;
#endif

#include <limits.h>
#include <string.h>
#include <stddef.h>

typedef int BOOL;

#ifdef __cplusplus
extern "C"
{
#endif

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
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

/* return positive form of a negative value, avoiding overflow */
/* T should be an unsigned type */
#define M3_POS(T, a) (((T)-((a) + 1)) + 1)

/* compat: not referenced by any backend */

#ifndef _WIN32
long
m3_div(long b, long a)
{
  typedef          long ST; /* signed type */
  typedef unsigned long UT; /* unsigned type */
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
#endif

/* compat: not referenced by gcc backend */

#ifdef _WIN32
int64 __stdcall m3_div64(int64 b, int64 a)
#else
int64 __stdcall m3_divL(int64 b, int64 a)
#endif
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

/* compat: not referenced by any backend */

#ifndef _WIN32
long __stdcall m3_mod(long b, long a)
{
  typedef          long ST; /* signed type */
  typedef unsigned long UT; /* unsigned type */
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

/* compat: not referenced by gcc backend */

#ifdef _WIN32
int64 __stdcall m3_mod64(int64 b, int64 a)
#else
int64 __stdcall m3_modL(int64 b, int64 a)
#endif
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

#define SET_GRAIN (sizeof (size_t) * 8)

/* compat: not referenced by any backend */

size_t __cdecl set_member(size_t bit_index, size_t* set)
{
  size_t word = bit_index / SET_GRAIN;
  size_t bit  = bit_index % SET_GRAIN;
  return (set[word] & (((size_t)1) << bit)) != 0;
}

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

#define HIGH_BITS(a) ((~(size_t)0) << (a))
#define LOW_BITS(a)  ((~(size_t)0) >> (SET_GRAIN - (a) - 1))

void __stdcall set_range
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
        s[i] = ~(size_t)0;
      s [b_word] |= low_bits;
    }
  }
}

/* compat: not referenced by any backend */

void __cdecl set_singleton(size_t bit_index, size_t* set)
{
  size_t word = bit_index / SET_GRAIN;
  size_t bit  = bit_index % SET_GRAIN;
  set[word] |= (((size_t)1) << bit);
}

#ifdef _WIN32

uint64 _rotl64(uint64 value, int shift);
uint64 _rotr64(uint64 value, int shift);
#pragma intrinsic(_rotl64)
#pragma intrinsic(_rotr64)
uint64 __stdcall  m3_rotate_left64(uint64 a, int b)  { return _rotl64(a, b); }
uint64 __stdcall m3_rotate_right64(uint64 a, int b)  { return _rotr64(a, b); }

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

#ifdef __cplusplus
} /* extern "C" */
#endif
