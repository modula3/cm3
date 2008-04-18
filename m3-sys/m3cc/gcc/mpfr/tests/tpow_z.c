/* Test file for mpfr_pow_z -- power function x^z with z a MPZ

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

#include <stdlib.h>
#include <float.h>
#include <math.h>

#include "mpfr-test.h"

#define ERROR(str) do { printf("Error for "str"\n"); exit (1); } while (0)

static void
check_special (void)
{
  mpfr_t x, y;
  mpz_t  z;
  int res;

  mpfr_init (x);
  mpfr_init (y);
  mpz_init (z);

  /* x^0 = 1 except for NAN */
  mpfr_set_ui (x, 23, GMP_RNDN);
  mpz_set_ui (z, 0);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_cmp_ui (y, 1) != 0)
    ERROR ("23^0");
  mpfr_set_nan (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_nan_p (y) == 0)
    ERROR ("NAN^0");
  mpfr_set_inf (x, 1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_cmp_ui (y, 1) != 0)
    ERROR ("INF^0");

  /* sINF^N = INF if s==1 or n even if N > 0*/
  mpz_set_ui (z, 42);
  mpfr_set_inf (x, 1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || mpfr_sgn (y) <= 0)
    ERROR ("INF^42");
  mpfr_set_inf (x, -1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || mpfr_sgn (y) <= 0)
    ERROR ("-INF^42");
  mpz_set_ui (z, 17);
  mpfr_set_inf (x, 1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || mpfr_sgn (y) <= 0)
    ERROR ("INF^17");
  mpfr_set_inf (x, -1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || mpfr_sgn (y) >= 0)
    ERROR ("-INF^17");

  mpz_set_si (z, -42);
  mpfr_set_inf (x, 1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("INF^-42");
  mpfr_set_inf (x, -1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("-INF^-42");
  mpz_set_si (z, -17);
  mpfr_set_inf (x, 1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("INF^-17");
  mpfr_set_inf (x, -1);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) >= 0)
    ERROR ("-INF^-17");

  /* s0^N = +0 if s==+ or n even if N > 0*/
  mpz_set_ui (z, 42);
  MPFR_SET_ZERO (x); MPFR_SET_POS (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("+0^42");
  MPFR_SET_NEG (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("-0^42");
  mpz_set_ui (z, 17);
  MPFR_SET_POS (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("+0^17");
  MPFR_SET_NEG (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_zero_p (y) == 0 || MPFR_SIGN (y) >= 0)
    ERROR ("-0^17");

  mpz_set_si (z, -42);
  MPFR_SET_ZERO (x); MPFR_SET_POS (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("+0^-42");
  MPFR_SET_NEG (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("-0^-42");
  mpz_set_si (z, -17);
  MPFR_SET_POS (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || MPFR_SIGN (y) <= 0)
    ERROR ("+0^-17");
  MPFR_SET_NEG (x);
  res = mpfr_pow_z (y, x, z, GMP_RNDN);
  if (res != 0 || mpfr_inf_p (y) == 0 || MPFR_SIGN (y) >= 0)
    ERROR ("-0^-17");

  mpz_clear (z);
  mpfr_clear (y);
  mpfr_clear (x);
}

static void
check_integer (mp_prec_t begin, mp_prec_t end, unsigned long max)
{
  mpfr_t x, y1, y2;
  mpz_t z;
  unsigned long i, n;
  mp_prec_t p;
  int res1, res2;
  mp_rnd_t rnd;

  mpfr_inits2 (begin, x, y1, y2, (void *) 0);
  mpz_init (z);
  for (p = begin ; p < end ; p+=4)
    {
      mpfr_set_prec (x, p);
      mpfr_set_prec (y1, p);
      mpfr_set_prec (y2, p);
      for (i = 0 ; i < max ; i++)
        {
          mpz_random (z, (i&1) == 0 ? -1 : 1);
          mpfr_random (x);
          mpfr_mul_2ui (x, x, 1, GMP_RNDN); /* 0 <= x < 2 */
          rnd = (mp_rnd_t) RND_RAND ();
          if (mpz_fits_slong_p (z))
            {
              n = mpz_get_si (z);
              /* printf ("New test for x=%ld\nCheck Pow_si\n", n); */
              res1 = mpfr_pow_si (y1, x, n, rnd);
              /* printf ("Check pow_z\n"); */
              res2 = mpfr_pow_z  (y2, x, z, rnd);
              if (mpfr_cmp (y1, y2) != 0)
                {
                  printf ("Error for p = %lu, z = %ld, rnd = %s and x = ",
                          p, n, mpfr_print_rnd_mode (rnd));
                  mpfr_dump (x);
                  printf ("Ypowsi = "); mpfr_dump (y1);
                  printf ("Ypowz  = "); mpfr_dump (y2);
                  exit (1);
                }
              if (res1 != res2)
                {
                  printf ("Wrong inexact flags for p = %lu, z = %ld, rnd = %s"
                          " and x = ", p, n, mpfr_print_rnd_mode (rnd));
                  mpfr_dump (x);
                  printf ("Ypowsi(inex = %2d) = ", res1); mpfr_dump (y1);
                  printf ("Ypowz (inex = %2d) = ", res2); mpfr_dump (y2);
                  exit (1);
                }
            }
        } /* for i */
    } /* for p */
  mpfr_clears (x, y1, y2, (void *) 0);
  mpz_clear (z);
}

static void
check_regression (void)
{
  mpfr_t x, y;
  mpz_t  z;
  int res1, res2;

  mpz_init_set_ui (z, 2026876995);
  mpfr_init2 (x, 122);
  mpfr_init2 (y, 122);

  mpfr_set_str_binary (x, "0.10000010010000111101001110100101101010011110011100001111000001001101000110011001001001001011001011010110110110101000111011E1");
  res1 = mpfr_pow_z (y, x, z, GMP_RNDU);
  res2 = mpfr_pow_ui (x, x, 2026876995UL, GMP_RNDU);
  if (mpfr_cmp (x, y) || res1 != res2)
    {
      printf ("Regression (1) tested failed (%d=?%d)\n",res1, res2);
      printf ("pow_ui: "); mpfr_dump (x);
      printf ("pow_z:  "); mpfr_dump (y);

      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpz_clear (z);
}

int
main (void)
{
  MPFR_TEST_USE_RANDS ();
  tests_start_mpfr ();

  check_special ();
  check_integer (2, 163, 100);
  check_regression ();

  tests_end_mpfr ();
  return 0;
}
