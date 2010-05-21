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

#include <stddef.h>

/* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.
 */
#if __INITIAL_POINTER_SIZE == 64
typedef unsigned __int64 WORD_T;
#else
typedef size_t WORD_T;
#endif

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

#ifdef _WIN32
int64
__stdcall
m3_div64(int64 b, int64 a)
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
#endif

#ifdef _WIN32
int64
__stdcall
m3_mod64(int64 b, int64 a)
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

#define SET_GRAIN (sizeof (WORD_T) * 8)

void
__stdcall
set_union(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  for (i = 0; i < n_words; i++)
    a[i] = b[i] | c[i];
}

void
__stdcall
set_intersection(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  for (i = 0; i < n_words; i++)
    a[i] = b[i] & c[i];
}

void
__stdcall
set_difference(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  for (i = 0; i < n_words; i++)
    a[i] = b[i] & (~ c[i]);
}

void
__stdcall
set_sym_difference(WORD_T n_bits, WORD_T* c, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  for (i = 0; i < n_words; i++)
    a[i] = b[i] ^ c[i];
}

WORD_T
__stdcall
set_le(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
  }
  return 1;
}

WORD_T
__stdcall
set_lt(WORD_T n_bits, WORD_T* b, WORD_T* a)
{
  register WORD_T n_words = n_bits / SET_GRAIN;
  register WORD_T i;
  register WORD_T eq = 0;
  for (i = 0; i < n_words; i++) {
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
  if (b < a) {
      /* no bits to set */
  } else {
    WORD_T a_word = a / SET_GRAIN;
    WORD_T b_word = b / SET_GRAIN;
    WORD_T i;
    WORD_T high_bits = HIGH_BITS(a % SET_GRAIN);
    WORD_T low_bits = LOW_BITS(b % SET_GRAIN);

    if (a_word == b_word) {
      s [a_word] |= (high_bits & low_bits);
    } else {
      s [a_word] |= high_bits;
      for (i = a_word + 1; i < b_word; ++i)
        s[i] = ~(WORD_T)0;
      s [b_word] |= low_bits;
    }
  }
}

#ifdef _WIN32

uint64 _rotl64(uint64 value, int shift);
uint64 _rotr64(uint64 value, int shift);
#pragma intrinsic(_rotl64)
#pragma intrinsic(_rotr64)

uint64
__stdcall
m3_rotate_left64(uint64 a, int b) { return _rotl64(a, b); }

uint64
__stdcall
m3_rotate_right64(uint64 a, int b) { return _rotr64(a, b); }

uint64
__stdcall
m3_rotate64(uint64 a, int b)
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
