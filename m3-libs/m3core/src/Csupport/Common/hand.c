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
#endif

#include "m3core.h"

#if __GNUC__ >= 4
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

/* return positive form of a negative value, avoiding overflow */
/* T should be an unsigned type */
#define M3_POS(T, a) (((T)-((a) + 1)) + 1)

#ifndef _WIN32
#define m3_div64 m3_divL
#define m3_mod64 m3_modL
#endif

#ifndef _WIN32

INTEGER
__stdcall
m3_div(INTEGER b, INTEGER a)
{
  typedef  INTEGER ST; /* signed type */
  typedef   WORD_T UT; /* unsigned type */
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

INTEGER
__stdcall
m3_mod(INTEGER b, INTEGER a)
{
  typedef  INTEGER ST; /* signed type */
  typedef   WORD_T UT; /* unsigned type */
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

INT64
__stdcall
m3_div64(INT64 b, INT64 a)
{
  typedef  INT64 ST; /* signed type */
  typedef UINT64 UT; /* unsigned type */
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

INT64
__stdcall
m3_mod64(INT64 b, INT64 a)
{
  typedef  INT64 ST; /* signed type */
  typedef UINT64 UT; /* unsigned type */
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

#define SET_GRAIN (sizeof(WORD_T) * 8)

WORD_T
__stdcall
set_member(WORD_T elt, WORD_T* set)
{
  WORD_T const word = elt / SET_GRAIN;
  WORD_T const bit  = elt % SET_GRAIN;
  return (set[word] & (((WORD_T)1) << bit)) != 0;
}

void
__stdcall
set_union(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  for (i = 0; i < n_words; ++i)
    a[i] = b[i] | c[i];
}

void
__stdcall
set_intersection(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  for (i = 0; i < n_words; ++i)
    a[i] = b[i] & c[i];
}

void
__stdcall
set_difference(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  for (i = 0; i < n_words; ++i)
    a[i] = b[i] & (~ c[i]);
}

void
__stdcall
set_sym_difference(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  for (i = 0; i < n_words; ++i)
    a[i] = b[i] ^ c[i];
}

WORD_T
__stdcall
set_eq(WORD_T n_bits, WORD_T* b, WORD_T* a)
/* never used by current backend */
{
  return (memcmp(a, b, n_bits / 8) == 0);
}

WORD_T
__stdcall
set_ne(WORD_T n_bits, WORD_T* b, WORD_T* a)
/* never used by current backend */
{
  return (memcmp(a, b, n_bits / 8) != 0);
}

WORD_T
__stdcall
set_le(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  for (i = 0; i < n_words; ++i) {
    if (a[i] & (~ b[i])) return 0;
  }
  return 1;
}

WORD_T
__stdcall
set_lt(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  WORD_T const n_words = n_bits / SET_GRAIN;
  WORD_T i;
  WORD_T eq = 0;
  for (i = 0; i < n_words; ++i) {
    if (a[i] & (~ b[i])) return 0;
    eq |= (a[i] ^ b[i]);
  }
  return (eq != 0);
}

WORD_T
__stdcall
set_ge(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  return set_le(n_bits, a, b);
}

WORD_T
__stdcall
set_gt(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  return set_lt(n_bits, a, b);
}

#define HIGH_BITS(a) ((~(WORD_T)0) << (a))
#define LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))

void
__stdcall
set_range(WORD_T b, WORD_T a, WORD_T* s)
{
  if (a > b) {
      /* no bits to set */
  } else {
    WORD_T const a_word = a / SET_GRAIN;
    WORD_T const b_word = b / SET_GRAIN;
    WORD_T i;
    WORD_T const high_bits = HIGH_BITS(a % SET_GRAIN);
    WORD_T const low_bits = LOW_BITS(b % SET_GRAIN);

    if (a_word == b_word) {
      s[a_word] |= (high_bits & low_bits);
    } else {
      s[a_word] |= high_bits;
      for (i = a_word + 1; i < b_word; ++i)
        s[i] = ~(WORD_T)0;
      s[b_word] |= low_bits;
    }
  }
}

void
__stdcall
set_singleton(WORD_T a, WORD_T* s)
{
  WORD_T a_word = a / SET_GRAIN;
  WORD_T a_bit  = a % SET_GRAIN;
  s[a_word] |= (((WORD_T)1) << a_bit);
}

#ifdef _WIN32

UINT64 _rotl64(UINT64 value, int shift);
UINT64 _rotr64(UINT64 value, int shift);
#pragma intrinsic(_rotl64)
#pragma intrinsic(_rotr64)

UINT64
__stdcall
m3_rotate_left64(UINT64 a, int b) { return _rotl64(a, b); }

UINT64
__stdcall
m3_rotate_right64(UINT64 a, int b) { return _rotr64(a, b); }

UINT64
__stdcall
m3_rotate64(UINT64 a, int b)
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
