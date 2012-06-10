/* Test file for mpfr_set.

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

/* Maybe better create its own test file ? */
static void
check_neg_special (void)
{
  mpfr_t x;
  mpfr_init (x);
  MPFR_SET_NAN (x);
  mpfr_clear_nanflag ();
  mpfr_neg (x, x, GMP_RNDN);
  if (!mpfr_nanflag_p () )
    {
      printf("ERROR: neg (NaN) doesn't set Nan flag.\n");
      exit (1);
    }
  mpfr_clear (x);
}

#define TEST_FUNCTION mpfr_set
#include "tgeneric.c"

int
main (void)
{
  mp_prec_t p, q;
  mpfr_t x, y, z, u;
  int rnd;
  int inexact, cmp;
  mp_exp_t emax;

  tests_start_mpfr ();

  /* check prototypes of mpfr_init_set_* */
  inexact = mpfr_init_set_si (x, -1, GMP_RNDN);
  inexact = mpfr_init_set (y, x, GMP_RNDN);
  inexact = mpfr_init_set_ui (z, 1, GMP_RNDN);
  inexact = mpfr_init_set_d (u, 1.0, GMP_RNDN);

  mpfr_set_nan (x);
  (mpfr_set) (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_nan_p (y));

  mpfr_set_inf (x, 1);
  mpfr_set (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (y) && mpfr_sgn (y) > 0);

  mpfr_set_inf (x, -1);
  mpfr_set (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (y) && mpfr_sgn (y) < 0);

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_set (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_cmp_ui (y, 0) == 0 && MPFR_IS_POS(y));

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_set (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_cmp_ui (y, 0) == 0 && MPFR_IS_NEG(y));

  emax = mpfr_get_emax ();
  set_emax (0);
  mpfr_set_prec (x, 3);
  mpfr_set_str_binary (x, "0.111");
  mpfr_set_prec (y, 2);
  mpfr_set (y, x, GMP_RNDU);
  if (!(MPFR_IS_INF (y) && MPFR_SIGN (y) > 0))
    {
      printf ("Error for y=x=0.111 with px=3, py=2 and emax=0\nx=");
      mpfr_dump (x);
      printf ("y=");
      mpfr_dump (y);
      exit (1);
    }

  MPFR_ASSERTN (MPFR_IS_INF (y) && MPFR_SIGN (y) > 0);
  set_emax (emax);

  mpfr_set_prec (y, 11);
  mpfr_set_str_binary (y, "0.11111111100E-8");
  mpfr_set_prec (x, 2);
  mpfr_set (x, y, GMP_RNDN);
  mpfr_set_str_binary (y, "1.0E-8");
  if (mpfr_cmp (x, y))
    {
      printf ("Error for y=0.11111111100E-8, prec=2, rnd=GMP_RNDN\n");
      exit (1);
    }

  for (p=2; p<500; p++)
    {
      mpfr_set_prec (x, p);
      mpfr_urandomb (x, RANDS);
      if (randlimb () % 2)
        mpfr_neg (x, x, GMP_RNDN);
      for (q=2; q<2*p; q++)
        {
          mpfr_set_prec (y, q);
          for (rnd = 0; rnd < GMP_RND_MAX; rnd++)
            {
              inexact = mpfr_set (y, x, (mp_rnd_t) rnd);
              cmp = mpfr_cmp (y, x);
              if (((inexact == 0) && (cmp != 0)) ||
                  ((inexact > 0) && (cmp <= 0)) ||
                  ((inexact < 0) && (cmp >= 0)))
                {
                  printf ("Wrong inexact flag in mpfr_set: expected %d,"
                          " got %d\n", cmp, inexact);
                  exit (1);
                }
            }
        }
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
  mpfr_clear (u);

  check_neg_special ();

  test_generic (2, 1000, 10);

  tests_end_mpfr ();
  return 0;
}
