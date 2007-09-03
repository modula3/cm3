
/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

#ifdef __cplusplus
extern "C"
{           
#endif

typedef unsigned long ulong;
typedef unsigned int uint;

#ifdef _MSC_VER
#pragma warning(disable:4131) /* old style */
    #if _MSC_VER < 900
        #error __int64 support is required.
        /* avoid cascade */
        typedef long longlong;
    #else
        typedef __int64 longlong;
    #endif
#else
        typedef long long longlong;
#endif

#ifdef __cplusplus
#define ANSI(x) x
#define KR(x)
#else
#define ANSI(x)
#define KR(x) x
#endif

long m3_div
    ANSI((      long b, long a))
      KR((b, a) long b; long a;)
{
  register long c;
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

longlong m3_divL
    ANSI((      longlong b, longlong a))
      KR((b, a) longlong b; longlong a;)
{
  register longlong c;
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

long m3_mod
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

longlong m3_modL
    ANSI((      longlong b, longlong a))
      KR((b, a) longlong b; longlong a;)
{
  register longlong c;
  if ((a == 0) && (b != 0)) {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? a % b : b + 1 + (a-1) % (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? b - 1 - (-1-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

#define SET_GRAIN (sizeof (long) * 8)

long set_member
    ANSI((          long elt, ulong* set))
      KR((elt, set) long elt; ulong* set;)
{
  register long word = elt / SET_GRAIN;
  register long bit  = elt % SET_GRAIN;
  return (set[word] & (1UL << bit)) != 0;
}

void set_union
    ANSI((                 long n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) long n_bits; ulong* c; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] | c[i];
  }
}

void set_intersection
    ANSI((                 long n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) long n_bits; ulong* c; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & c[i];
  }
}

void set_difference
    ANSI((                 long n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) long n_bits; ulong* c; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] & (~ c[i]);
  }
}

void set_sym_difference
    ANSI((                 long n_bits, ulong* c, ulong* b, ulong* a))
      KR((n_bits, c, b, a) long n_bits; ulong* c; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    a[i] = b[i] ^ c[i];
  }
}

long set_eq
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    if (a[i] != b[i]) return 0;
  }
  return 1;
}

long set_ne
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    if (a[i] != b[i]) return 1;
  }
  return 0;
}

long set_ge
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
  }
  return 1;
}

long set_gt
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0;
  for (i = 0; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0;
    eq |=  (a[i] ^ b[i]);
  }
  return (eq != 0);
}

long set_le
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0;
  }
  return 1;
}

long set_lt
    ANSI((              long n_bits, ulong* b, ulong* a))
      KR((n_bits, b, a) long n_bits; ulong* b; ulong* a;)
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0;
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

static volatile int tables_built;
static unsigned long LoBits[SET_GRAIN];  /* LoBits [i] = SET { 0..i } */
static unsigned long HiBits[SET_GRAIN];  /* HiBits [i] = SET { i..GRAIN-1 } */

void BuildTables ()
{
  unsigned i;
  ulong j;

  /* LoBits [i] = SET { 0..i } */
  j = 0;  /* == SET { } */
  for (i = 0; i < SET_GRAIN; i++) {
    j = (j << 1) + 1;
    LoBits[i] = j;
  }

  /* HiBits [i] = SET { i..GRAIN-1 } */
  j = ~0UL; /* == SET { 0..GRAIN-1 } */
  for (i = 0; i < SET_GRAIN; i++) {
    HiBits[i] = j;
    j = (j << 1);
  }

  tables_built = 1;
}

void set_range
    ANSI((       long b, long a, ulong* s))
    KR((b, a, s) long b; long a; ulong* s;)
{
  if (b < a) {
      /* no bits to set */
  } else {
      long a_word = a / SET_GRAIN;
      long a_bit  = a % SET_GRAIN;
      long b_word = b / SET_GRAIN;
      long b_bit  = b % SET_GRAIN;
      long i;

      if (!tables_built) BuildTables ();

      if (a_word == b_word) {
          s [a_word] |= (HiBits [a_bit] & LoBits [b_bit]);
      } else {
          s [a_word] |= HiBits [a_bit];
          for (i = a_word+1; i < b_word; i++)  s[i] = ~0UL;
          s [b_word] |= LoBits [b_bit];
      }
    }
}

void set_singleton
    ANSI((      long a, ulong* s))
      KR((a, s) long a; ulong* s;)
{
  long a_word = a / SET_GRAIN;
  long a_bit  = a % SET_GRAIN;
  s[a_word] |= (1UL << a_bit);
}

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


#if 0 /* work in progress */

#include <assert.h>

int __cdecl wmain()
{
    unsigned i;

    BuildTables();
    for (i = 0 ; i != 32 ; ++i)
    {
        assert(LoBits[i] == _lowbits[i + 1]);
        assert(HiBits[i] == _highbits[i]);
    }

    return 0;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
