/* __gmpfr_isqrt && __gmpfr_cuberoot -- Integer square root and cube root

Copyright 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the MPFR Library.

The MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include "mpfr-impl.h"

/* returns floor(sqrt(n)) */
unsigned long
__gmpfr_isqrt (unsigned long n)
{
  unsigned long s;

  s = 1;
  do {
    s = (s + n / s) / 2;
  } while (!(s*s <= n && n <= s*(s+2)));
  return s;
}

/* returns floor(n^(1/3)) */
unsigned long
__gmpfr_cuberoot (unsigned long n)
{
  double s, is;

  s = 1.0;
  do {
    s = (2*s*s*s + (double) n) / (3*s*s);
    is = (double) ((int) s);
  } while (!(is*is*is <= (double) n && (double) n < (is+1)*(is+1)*(is+1)));
  return (unsigned long) is;
}

