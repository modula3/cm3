/* Test file for mpfr_subnormalize.

Copyright 2005, 2006, 2007 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

static const struct {
  const char *in;
  int i;
  mp_rnd_t rnd;
  const char *out;
} tab[] ={
  {"1E1",  0, GMP_RNDN, "1E1"},
  {"1E1", -1, GMP_RNDZ, "1E1"},
  {"1E1", -1, GMP_RNDD, "1E1"},
  {"1E1",  1, GMP_RNDU, "1E1"},
  {"0.10000E-10", 0, GMP_RNDN, "0.1E-10"},
  {"0.10001E-10", 0, GMP_RNDN, "0.1E-10"},
  {"0.11001E-10", 0, GMP_RNDN, "0.1E-9"},
  {"0.11001E-10", 0, GMP_RNDZ, "0.1E-10"},
  {"0.11001E-10", 0, GMP_RNDU, "0.1E-9"},
  {"0.11000E-10", 0, GMP_RNDN, "0.1E-9"},
  {"0.11111E-8", 0, GMP_RNDN, "0.10E-7"},
  {"0.10111E-8", 0, GMP_RNDN, "0.11E-8"},
  {"0.11110E-8", -1, GMP_RNDN, "0.10E-7"},
  {"0.10110E-8", 1, GMP_RNDN, "0.101E-8"}
};

static void
check1 (void)
{
  mpfr_t x;
  int i, j;

  mpfr_set_default_prec (9);
  mpfr_set_emin (-10);
  mpfr_set_emax (10);

  mpfr_init (x);
  for (i = 0; i < (sizeof (tab) / sizeof (tab[0])); i++)
    {
      mpfr_set_str (x, tab[i].in, 2, GMP_RNDN);
      j = mpfr_subnormalize (x, tab[i].i, tab[i].rnd);
      if (mpfr_cmp_str (x, tab[i].out, 2, GMP_RNDN) != 0)
        {
          printf ("Error for i=%d\nFor:%s\nExpected:%s\nGot:",
                  i, tab[i].in, tab[i].out);
          mpfr_dump (x);
          exit (1);
        }
    }
  mpfr_clear (x);
}


int
main (int argc, char *argv[])
{
  tests_start_mpfr ();

  check1 ();

  tests_end_mpfr ();
  return 0;
}
