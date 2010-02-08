/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

typedef unsigned int uint;
typedef unsigned long ulong;

#ifdef _WIN32
#define WIN32_STATIC static
#else
#define WIN32_STATIC
#endif

#define NOT_YET static

#ifdef _MSC_VER
#pragma warning(disable:4255) /* () changed to (void) */
#pragma warning(disable:4505) /* unused static function removed */
#pragma warning(disable:4711) /* automatic inlining */
#undef _DLL
#ifndef _MT
#define _MT
#endif
#if _MSC_VER < 900
#error __int64 support is required.
/* avoid cascade */
typedef long int64;
typedef ulong uint64;
#else
typedef __int64 int64;
typedef unsigned __int64 uint64;
#define I64 "I64"
#endif
#else
typedef long long int64;
typedef unsigned long long uint64;
#define I64 "ll"
#endif

#include <limits.h>
#include <string.h>
#include <assert.h>

#if !defined(INT64_MAX)
#if defined(LLONG_MAX)
#define INT64_MAX LLONG_MAX
#elif defined(__LONG_LONG_MAX__)
#define INT64_MAX __LONG_LONG_MAX__
#elif defined(_I64_MAX)
#define INT64_MAX _I64_MAX
#endif
#endif

#if !defined(INT64_MIN)
#if defined(LLONG_MIN)
#define INT64_MIN LLONG_MIN
#elif defined(__LONG_LONG_MIN__)
#define INT64_MIN __LONG_LONG_MIN__
#elif defined(_I64_MIN)
#define INT64_MIN _I64_MIN
#elif defined(INT64_MAX)
#define INT64_MIN (-INT64_MAX-(int64)1)
#endif
#endif

typedef int BOOL;

#ifdef __cplusplus
extern "C"
{           
#endif

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

#if UCHAR_MAX == 0xffffffff
typedef unsigned char uint32;
#elif USHRT_MAX == 0xffffffff
typedef unsigned short uint32;
#elif UINT_MAX == 0xffffffff
typedef uint uint32;
#elif ULONG_MAX == 0xffffffff
typedef ulong uint32;
#else
#error no 32 bit integer type
#endif

/* There are problems passing int64 in K&R form! */
#if 1 /* defined(__STDC__) || defined(__cplusplus) || defined(_MSC_VER) */
#define ANSI(x) x
#define KR(x)
#else
#define ANSI(x)
#define KR(x) x
#endif

NOT_YET int __stdcall m3_add(int a, int b, BOOL* overflow)
{
  int c = (a + b);
  BOOL asign = (a < 0);
  /* positive + positive: expect positive
     negative + negative: expect negative
     positive + negative: cannot overflow
     overflow if input signs equal and output doesn't match them */
  *overflow |= (asign == (b < 0) && asign != (c < 0));
  return c;
}

NOT_YET int64 __stdcall m3_add_64(int64 a, int64 b, BOOL* overflow)
{
  int64 c = (a + b);
  BOOL asign = (a < 0);
  /* positive + positive: expect positive
     negative + negative: expect negative
     positive + negative: cannot overflow
     overflow if input signs equal and output doesn't match them */
  *overflow |= (asign == (b < 0) && asign != (c < 0));
  return c;
}

NOT_YET int __stdcall m3_sub(int a, int b, BOOL* overflow)
{
  int c = (a - b);
  BOOL asign = (a < 0);
  /* positive - positive: cannot overflow
     negative - negative: cannot overflow
     positive - negative: expect positive
     negative - positive: expect negative
     overflow if input signs vary and output doesn't match first input */
  *overflow |= (asign != (b < 0) && asign != (c < 0));
  return c;
}

NOT_YET int64 __stdcall m3_sub_64(int64 a, int64 b, BOOL* overflow)
{
  int64 c = (a - b);
  BOOL asign = (a < 0);
  /* positive - positive: cannot overflow
     negative - negative: cannot overflow
     positive - negative: expect positive
     negative - positive: expect negative
     overflow if input signs vary and output doesn't match first input */
  *overflow |= (asign != (b < 0) && asign != (c < 0));
  return c;
}

NOT_YET int __stdcall m3_mult(int a, int b, BOOL* overflow)
{
  /* do work in higher precision and range check result */
  int64 c = (a * (int64)b);
  *overflow |= (c < INT_MIN || c > INT_MAX);
  return (int)c;
}

NOT_YET uint __stdcall m3_add_u(uint a, uint b, BOOL* overflow)
{
  uint c = (a + b);
  /* overflow if output less than either input */
  *overflow |= (c < a);
  return c;
}

NOT_YET uint64 __stdcall m3_add_u64(uint64 a, uint64 b, BOOL* overflow)
{
  uint64 c = (a + b);
  /* overflow if output less than either input */
  *overflow |= (c < a);
  return c;
}

NOT_YET uint __stdcall m3_sub_u(uint a, uint b, BOOL* overflow)
{
  uint c = (a - b);
  /* overflow if output greater than first input */
  *overflow |= (c > a);
  return c;
}

NOT_YET uint64 __stdcall m3_sub_u64(uint64 a, uint64 b, BOOL* overflow)
{
  uint64 c = (a - b);
  /* overflow if output greater than first input */
  *overflow |= (c > a);
  return c;
}

NOT_YET uint __stdcall m3_mult_u(uint a, uint b, BOOL* overflow)
{
  /* do work in higher precision and range check result */
  uint64 c = (a * (uint64)b);
  *overflow |= (c > UINT_MAX);
  return (uint)c;
}

NOT_YET uint64 __stdcall m3_mult_u64(uint64 a, uint64 b, BOOL* overflow)
{
  /* break it down into smaller steps
  hi(x) = x >> 32
  lo(x) = (uint32)x
  result =    (hi(a) * hi(b)) << 64
            + (hi(a) * lo(b)) << 32
            + (lo(a) * hi(b)) << 32
            + (a * b)
  checking for overflow on the additions and shifts
  */
  uint32 ahi;
  uint32 alo;
  uint32 bhi;
  uint32 blo;
  uint64 c;
  uint64 result = a * b;

  if (*overflow)
    return result;

  ahi = (uint32)(a >> 32);
  bhi = (uint32)(b >> 32);

  if (ahi && bhi)
    goto ov;

  alo = (uint32)a;
  blo = (uint32)b;
 
  c = m3_add_u64(alo * (uint64)bhi, ahi * (uint64)blo, overflow);
  if (*overflow)
    return result;

  if ((c >> 32) != 0)
    goto ov;

  m3_add_u64(alo * (uint64)blo, c << 32, overflow);
  return result;

ov:
  *overflow = 1;
  return result;
}


/* return positive form of a negative value, avoiding overflow */
/* T should be an unsigned type */
#define M3_POS(T, a) (((T)-((a) + 1)) + 1)
#define M3_ABS(T, a) (((a) < 0) ? M3_POS(T, a) : (T)(a))

static uint64 m3_abs64(int64 a) { return M3_ABS(uint64, a); }

NOT_YET int64 __stdcall m3_mult_64(int64 a, int64 b, BOOL* overflow)
{
  /* do the unsigned operation on the magnitudes
    overflow if it overflows
    range check result for smaller signed range,
    figuring the range based on the input signs */
  uint64 c;
  int64 result = a * b;

  if (*overflow)
    return result;

  c = m3_mult_u64(m3_abs64(a), m3_abs64(b), overflow);
  if (*overflow)
    return result;

  if ((a < 0) == (b < 0))
    *overflow |= (c > (uint64)INT64_MAX);
  else
    *overflow |= (c > M3_POS(uint64, INT64_MIN));
  
  return result;
}

static long __stdcall m3_div_old
    ANSI((      long b, long a))
      KR((b, a) long b; long a;)
{
  register long c;
  
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ { c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

static int64 __stdcall m3_divL_old
    ANSI((      int64 b, int64 a))
      KR((b, a) int64 b; int64 a;)
{
  register int64 c;
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ { c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

WIN32_STATIC long __stdcall m3_div
    ANSI((      long b, long a))
      KR((b, a) long b; long a;)
{
  typedef  long ST; /* signed type */
  typedef ulong UT; /* unsigned type */
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

static long __stdcall m3_mod_old
    ANSI((      long b, long a))
      KR((b, a) long b; long a;)
{
  register long c;
  if ((a == 0) && (b != 0)) {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? a % b : b + 1 + (a-1) % (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? b - 1 - (-1-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

static int64 __stdcall m3_modL_old
    ANSI((      int64 b, int64 a))
      KR((b, a) int64 b; int64 a;)
{
  register int64 c;
  if ((a == 0) && (b != 0)) {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? a % b : b + 1 + (a-1) % (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? b - 1 - (-1-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

WIN32_STATIC long __stdcall m3_mod
    ANSI((      long b, long a))
      KR((b, a) long b; long a;)
{
  typedef  long ST; /* signed type */
  typedef ulong UT; /* unsigned type */
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

#define SET_GRAIN (sizeof (long) * 8)

ulong __stdcall set_member
    ANSI((          ulong elt, ulong* set))
      KR((elt, set) ulong elt; ulong* set;)
{
  register ulong word = elt / SET_GRAIN;
  register ulong bit  = elt % SET_GRAIN;
  return (set[word] & (1UL << bit)) != 0;
}

void __stdcall set_union
    ANSI((                 ulong n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) ulong n_bits; ulong* c; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] | c[i];
  }
}

void __stdcall set_intersection
    ANSI((                 ulong n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) ulong n_bits; ulong* c; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & c[i];
  }
}

void __stdcall set_difference
    ANSI((                 ulong n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) ulong n_bits; ulong* c; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & (~ c[i]);
  }
}

void __stdcall set_sym_difference
    ANSI((                 ulong n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) ulong n_bits; ulong* c; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] ^ c[i];
  }
}

ulong __stdcall set_eq
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
/* The integrated back end calls memcmp directly; the gcc
   backend does not. This could be put under #ifndef, but which?
   _WIN32 would break I386_MINGWIN. _MSC_VER would break
   Interix+gcc backend+Visual C+. Definitely consider
   changing the gcc backend though. */
{
  return (memcmp(a, b, n_bits / 8) == 0);
}

ulong __stdcall set_ne
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
/* The integrated back end calls memcmp directly; the gcc
   backend does not. This could be put under #ifndef, but which?
   _WIN32 would break I386_MINGWIN. _MSC_VER would break
   Interix+gcc backend+Visual C+. Definitely consider
   changing the gcc backend though. */
{
  return (memcmp(a, b, n_bits / 8) != 0);
}

ulong __stdcall set_ge
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
  }
  return 1;
}

ulong __stdcall set_gt
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  register ulong eq = 0;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
    eq |=  (a[i] ^ b[i]);
  }
  return (eq != 0);
}

ulong __stdcall set_le
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
  }
  return 1;
}

ulong __stdcall set_lt
    ANSI((              ulong n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) ulong n_bits; ulong* b; ulong* a;)
{
  register ulong n_words = n_bits / SET_GRAIN;
  register ulong i;
  register ulong eq = 0;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
    eq |= (a[i] ^ b[i]);
  }
  return (eq != 0);
}

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

#define LOW_BITS_ADJUST 0

#if ULONG_MAX == 0xffffffff

#if 1 /* UINT == 0xffffffff */

/* If possible, combine tables.
   If unsigned long and unsigned int are both 32 bit integers, then
    LoBits and _lowbits are the same except that _lowbits has 0 inserted in the first entry,
    HiBits and _highbits are the same except _highbits has 0 added at the end.
	That is, LoBits can be replaced with _lowbits if you add one to the index.
	HiBits can be replaced by _highbits directly.

  Now, a) it does not matter if unsigned int is exactly 32 bits, it need only be
  at least 32 bits b) it can't be larger than unsigned long anyway. So drop the condition
  and always combine these tables, for 32 bit unsigned long.
*/
#undef LOW_BITS_ADJUST 
#define LOW_BITS_ADJUST 1
#define LoBits _lowbits
#define HiBits _highbits

#else

static const ulong LoBits[] = {
0x00000001,0x00000003,0x00000007,0x0000000f,
0x0000001f,0x0000003f,0x0000007f,0x000000ff,
0x000001ff,0x000003ff,0x000007ff,0x00000fff,
0x00001fff,0x00003fff,0x00007fff,0x0000ffff,
0x0001ffff,0x0003ffff,0x0007ffff,0x000fffff,
0x001fffff,0x003fffff,0x007fffff,0x00ffffff,
0x01ffffff,0x03ffffff,0x07ffffff,0x0fffffff,
0x1fffffff,0x3fffffff,0x7fffffff,0xffffffff
};

static const ulong HiBits[] = {
0xffffffff,0xfffffffe,0xfffffffc,0xfffffff8,
0xfffffff0,0xffffffe0,0xffffffc0,0xffffff80,
0xffffff00,0xfffffe00,0xfffffc00,0xfffff800,
0xfffff000,0xffffe000,0xffffc000,0xffff8000,
0xffff0000,0xfffe0000,0xfffc0000,0xfff80000,
0xfff00000,0xffe00000,0xffc00000,0xff800000,
0xff000000,0xfe000000,0xfc000000,0xf8000000,
0xf0000000,0xe0000000,0xc0000000,0x80000000
};

#endif

#elif ULONG_MAX == 0xffffffffffffffff

static const ulong LoBits[] = {
0x0000000000000001,0x0000000000000003,0x0000000000000007,0x000000000000000f,
0x000000000000001f,0x000000000000003f,0x000000000000007f,0x00000000000000ff,
0x00000000000001ff,0x00000000000003ff,0x00000000000007ff,0x0000000000000fff,
0x0000000000001fff,0x0000000000003fff,0x0000000000007fff,0x000000000000ffff,
0x000000000001ffff,0x000000000003ffff,0x000000000007ffff,0x00000000000fffff,
0x00000000001fffff,0x00000000003fffff,0x00000000007fffff,0x0000000000ffffff,
0x0000000001ffffff,0x0000000003ffffff,0x0000000007ffffff,0x000000000fffffff,
0x000000001fffffff,0x000000003fffffff,0x000000007fffffff,0x00000000ffffffff,
0x00000001ffffffff,0x00000003ffffffff,0x00000007ffffffff,0x0000000fffffffff,
0x0000001fffffffff,0x0000003fffffffff,0x0000007fffffffff,0x000000ffffffffff,
0x000001ffffffffff,0x000003ffffffffff,0x000007ffffffffff,0x00000fffffffffff,
0x00001fffffffffff,0x00003fffffffffff,0x00007fffffffffff,0x0000ffffffffffff,
0x0001ffffffffffff,0x0003ffffffffffff,0x0007ffffffffffff,0x000fffffffffffff,
0x001fffffffffffff,0x003fffffffffffff,0x007fffffffffffff,0x00ffffffffffffff,
0x01ffffffffffffff,0x03ffffffffffffff,0x07ffffffffffffff,0x0fffffffffffffff,
0x1fffffffffffffff,0x3fffffffffffffff,0x7fffffffffffffff,0xffffffffffffffff
};

static const ulong HiBits[] = {
0xffffffffffffffff,0xfffffffffffffffe,0xfffffffffffffffc,0xfffffffffffffff8,
0xfffffffffffffff0,0xffffffffffffffe0,0xffffffffffffffc0,0xffffffffffffff80,
0xffffffffffffff00,0xfffffffffffffe00,0xfffffffffffffc00,0xfffffffffffff800,
0xfffffffffffff000,0xffffffffffffe000,0xffffffffffffc000,0xffffffffffff8000,
0xffffffffffff0000,0xfffffffffffe0000,0xfffffffffffc0000,0xfffffffffff80000,
0xfffffffffff00000,0xffffffffffe00000,0xffffffffffc00000,0xffffffffff800000,
0xffffffffff000000,0xfffffffffe000000,0xfffffffffc000000,0xfffffffff8000000,
0xfffffffff0000000,0xffffffffe0000000,0xffffffffc0000000,0xffffffff80000000,
0xffffffff00000000,0xfffffffe00000000,0xfffffffc00000000,0xfffffff800000000,
0xfffffff000000000,0xffffffe000000000,0xffffffc000000000,0xffffff8000000000,
0xffffff0000000000,0xfffffe0000000000,0xfffffc0000000000,0xfffff80000000000,
0xfffff00000000000,0xffffe00000000000,0xffffc00000000000,0xffff800000000000,
0xffff000000000000,0xfffe000000000000,0xfffc000000000000,0xfff8000000000000,
0xfff0000000000000,0xffe0000000000000,0xffc0000000000000,0xff80000000000000,
0xff00000000000000,0xfe00000000000000,0xfc00000000000000,0xf800000000000000,
0xf000000000000000,0xe000000000000000,0xc000000000000000,0x8000000000000000
};

#else
#error unknown size of ulong
#endif

void __stdcall set_range
    ANSI((       ulong b, ulong a, ulong* s))
    KR((b, a, s) ulong b; ulong a; ulong* s;)
{
  if (b < a) {
      /* no bits to set */
  } else {
      ulong a_word = a / SET_GRAIN;
      ulong a_bit  = a % SET_GRAIN;
      ulong b_word = b / SET_GRAIN;
      ulong b_bit  = b % SET_GRAIN;
      ulong i;

      if (a_word == b_word) {
          s [a_word] |= (HiBits [a_bit] & LoBits [b_bit + LOW_BITS_ADJUST]);
      } else {
          s [a_word] |= HiBits [a_bit];
          for (i = a_word+1; i < b_word; i++)  s[i] = ~0UL;
          s [b_word] |= LoBits [b_bit + LOW_BITS_ADJUST];
      }
    }
}

#define HIGH_BITS(a) ((~(ulong)0) << (a))
#define LOW_BITS(a)  ((~(ulong)0) >> (SET_GRAIN - (a) - 1))

static void __stdcall set_range_new
    ANSI((       ulong b, ulong a, ulong* s))
    KR((b, a, s) ulong b; ulong a; ulong* s;)
{
  if (b < a) {
      /* no bits to set */
  } else {
      ulong a_word = a / SET_GRAIN;
      ulong b_word = b / SET_GRAIN;
      ulong i;
      ulong high_bits = HIGH_BITS(a % SET_GRAIN);
      ulong low_bits = LOW_BITS(b % SET_GRAIN);

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

void __stdcall set_singleton
    ANSI((      ulong a, ulong* s))
      KR((a, s) ulong a; ulong* s;)
{
  ulong a_word = a / SET_GRAIN;
  ulong a_bit  = a % SET_GRAIN;
  s[a_word] |= (1UL << a_bit);
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

uint64 __stdcall m3_shift64(uint64 a, int b)
{
    if (b >= 64 || b <= -64)
        a = 0;
    else if (b > 0)
        a <<= b;
    else if (b < 0)
        a >>= -b;
    return a;
}

uint64 __stdcall m3_rotate64(uint64 a, int b)
{
    b &= 63;
    if (b > 0)
        a = _rotl64(a, b);
    else if (b < 0)
        a = _rotr64(a, -b);
    return a;
}

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
T __stdcall extract(T x, uint i, uint n)        \
{                                               \
    assert((n + i) <= (sizeof(T) * 8));         \
    x >>= i;                                    \
    x &= ~((~(T)0) << n);                       \
    return x;                                   \
}                                               \
                                                \
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
T __stdcall insert(T x, T y, uint i, uint n)    \
{                                               \
    T mask = ((~((~(T)0) << n)) << i);          \
    assert((n + i) <= (sizeof(T) * 8));         \
    return (x & ~mask) | ((y << i) & mask);     \
}                                               \

M3_EXTRACT_INSERT(m3_extract64, m3_extract_and_sign_extend64, m3_insert64, uint64)

#endif /* WIN32 */


/************************************************************************

#include <stdio.h>

static _crash (msg)
char *msg;
{
  fprintf (stderr, "\n**** UNIMPLEMENTED: %s ***\n", msg);
  fflush (stderr);

  *((long*)0L) = 1L;    /  * bad memory reference => crash! *  /
  while (1L);           /  * if not, loop forever           *  /
}

_xx0 () { _crash ("_xx0 (runtime fault)"); }

**************************************************************************/

#ifdef __cplusplus
} /* extern "C" */
#endif
