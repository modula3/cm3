/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

long m3_div (b, a)
long a, b;
{
  register long c;
  if ((a == 0L) && (b != 0L)) {  c = 0L;
  } else if (a > 0L)  {  c = (b >= 0L) ? (a) / (b) : -1L - (a-1L) / (-b);
  } else /* a < 0L */ {  c = (b >= 0L) ? -1L - (-1L-a) / (b) : (-a) / (-b);
  }
  return c;
}

long m3_mod (b, a)
long a, b;
{
  register long c;
  if ((a == 0L) && (b != 0L)) {  c = 0L;
  } else if (a > 0L)  {  c = (b >= 0L) ? a % b : b + 1L + (a-1L) % (-b);
  } else /* a < 0L */ {  c = (b >= 0L) ? b - 1L - (-1L-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

#define SET_GRAIN (sizeof (long) * 8)

long set_member (elt, set)
long elt;
long* set;
{
  register long word = elt / SET_GRAIN;
  register long bit  = elt % SET_GRAIN;
  return (set[word] & (1L << bit)) != 0L;
}

void set_union (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] | c[i];
  }
}

void set_intersection (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] & c[i];
  }
}

void set_difference (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] & (~ c[i]);
  }
}

void set_sym_difference (n_bits, c, b, a)
long n_bits;
long *c, *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    a[i] = b[i] ^ c[i];
  }
}

long set_eq (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] != b[i]) return 0L;
  }
  return 1L;
}

long set_ne (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] != b[i]) return 1L;
  }
  return 0L;
}

long set_ge (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0L;
  }
  return 1L;
}

long set_gt (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0L;
  for (i = 0L; i < n_words; i++) {
    if ((~ a[i]) & b[i]) return 0L;
    eq |=  (a[i] ^ b[i]);
  }
  return (eq != 0L);
}

long set_le (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  for (i = 0L; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0L;
  }
  return 1L;
}

long set_lt (n_bits, b, a)
long n_bits;
long *b, *a;
{
  register long n_words = n_bits / SET_GRAIN;
  register long i;
  register long eq = 0L;
  for (i = 0L; i < n_words; i++) {
    if (a[i] & (~ b[i])) return 0L;
    eq |= (a[i] ^ b[i]);
  }
  return (eq != 0L);
}

long tables_built = 0L;
unsigned long LoBits[SET_GRAIN];  /* LoBits [i] = SET { 0..i } */
unsigned long HiBits[SET_GRAIN];  /* HiBits [i] = SET { i..GRAIN-1 } */

BuildTables ()
{
  long i, j, k;

  tables_built = 1L;

  /* LoBits [i] = SET { 0..i } */
  j = 0L;  /* == SET { } */
  for (i = 0L; i < SET_GRAIN; i++) {
    j = (j << 1L) + 1L;
    LoBits[i] = j;
  }

  /* HiBits [i] = SET { i..GRAIN-1 } */
  j = ~0L; /* == SET { 0..GRAIN-1 } */
  for (i = 0L; i < SET_GRAIN; i++) {
    HiBits[i] = j;
    j = (j << 1L);
  }
}

void set_range (b, a, s)
long b, a;
long *s;
{
  long a_word = a / SET_GRAIN;
  long a_bit  = a % SET_GRAIN;
  long b_word = b / SET_GRAIN;
  long b_bit  = b % SET_GRAIN;
  long i;

  if (!tables_built) BuildTables ();

  if (b < a) {
      /* no bits to set */
  } else if (a_word == b_word) {
      s [a_word] |= (HiBits [a_bit] & LoBits [b_bit]);
  } else {
      s [a_word] |= HiBits [a_bit];
      for (i = a_word+1L; i < b_word; i++)  s[i] = HiBits [0];
      s [b_word] |= LoBits [b_bit];
  }
}

void set_singleton (a, s)
long a;
long *s;
{
  long a_word = a / SET_GRAIN;
  long a_bit  = a % SET_GRAIN;
  s [a_word] |= 1L << a_bit;
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


