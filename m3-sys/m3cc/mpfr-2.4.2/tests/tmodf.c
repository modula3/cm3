/* Test file for mpfr_modf.

Copyright 2007, 2008, 2009 Free Software Foundation, Inc.
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

#if MPFR_VERSION >= MPFR_VERSION_NUM(2,4,0)

static void
check (const char *xis, const char *xfs, const char *xs,
       mp_prec_t xip, mp_prec_t xfp, mp_prec_t xp,
       int expected_return, mp_rnd_t rnd_mode)
{
  int inexact;
  mpfr_t xi, xf, x;

  mpfr_init2 (xi, xip);
  mpfr_init2 (xf, xfp);
  mpfr_init2 (x, xp);
  mpfr_set_str1 (x, xs);
  inexact = mpfr_modf (xi, xf, x, rnd_mode);
  if (mpfr_cmp_str1 (xi, xis))
    {
      printf ("mpfr_modf failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode(rnd_mode));
      printf ("got integer value: ");
      mpfr_out_str (stdout, 10, 0, xi, GMP_RNDN);
      printf ("\nexpected %s\n", xis);
      exit (1);
    }
  if (mpfr_cmp_str1 (xf, xfs))
    {
      printf ("mpfr_modf failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode(rnd_mode));
      printf ("got fractional value: ");
      mpfr_out_str (stdout, 10, 0, xf, GMP_RNDN);
      printf ("\nexpected %s\n", xfs);
      exit (1);
    }
  if (inexact != expected_return)
    {
      printf ("mpfr_modf failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode(rnd_mode));
      printf ("got return value: %d, expected %d\n", inexact, expected_return);
      exit (1);
    }
  mpfr_clears (xi, xf, x, (mpfr_ptr) 0);
}

static void
check_nans (void)
{
  mpfr_t  x, xi, xf;

  mpfr_init2 (x, 123);
  mpfr_init2 (xi, 123);
  mpfr_init2 (xf, 123);

  /* nan */
  mpfr_set_nan (x);
  mpfr_modf (xi, xf, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (xi));
  MPFR_ASSERTN (mpfr_nan_p (xf));

  /* +inf */
  mpfr_set_inf (x, 1);
  mpfr_modf (xi, xf, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (xi));
  MPFR_ASSERTN (mpfr_sgn (xi) > 0);
  MPFR_ASSERTN (mpfr_zero_p (xf));

  /* -inf */
  mpfr_set_inf (x, -1);
  mpfr_modf (xi ,xf, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (xi));
  MPFR_ASSERTN (mpfr_sgn (xi) < 0);
  MPFR_ASSERTN (mpfr_zero_p (xf));

  mpfr_clear (x);
  mpfr_clear (xi);
  mpfr_clear (xf);
}

int
main (int argc, char *argv[])
{
  tests_start_mpfr ();

  check_nans ();

  check ("61680","3.52935791015625e-1", "61680.352935791015625",
         53, 53, 53, 0, GMP_RNDZ);
  check ("-53968","-3.529052734375e-1", "-53970.352935791015625",
         13, 13, 53, 2, GMP_RNDZ);
  check ("61632","3.525390625e-1",      "61648.352935791015625",
         10, 10, 53, -2, GMP_RNDZ);
  check ("61680", "0", "61680",  53, 53, 53, 0, GMP_RNDZ);
  check ("-53968","0", "-53970", 13, 13, 53, 1, GMP_RNDZ);
  check ("-43392","0", "-43399", 13, 13, 53, 1, GMP_RNDU);
  check ("-52720","0", "-52719", 13, 13, 53, -1, GMP_RNDD);
  check ("61632", "0", "61648",  10, 10, 53, -1, GMP_RNDZ);

  tests_end_mpfr ();
  return 0;
}

#else

int
main (void)
{
  printf ("Warning! Test disabled for this MPFR version.\n");
  return 0;
}

#endif
