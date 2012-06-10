/* Test file for mpfr_get_f.

Copyright 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include <limits.h>

#include "mpfr-test.h"

/* Test that there is no lost of accuracy when converting a mpfr_t number
   into a mpf_t number (test with various precisions and exponents). */
static void
prec_test (void)
{
  int px, py;

  for (py = 3; py <= 136; py++)
    {
      mpfr_t y1, y2, y3;

      mpfr_init2 (y1, py);
      mpfr_init2 (y2, py);
      mpfr_init2 (y3, py);

      for (px = 32; px <= 160; px += 32)
        {
          mpf_t x1, x2, x3;
          int e;

          mpf_init (x1);
          mpf_init (x2);
          mpf_init (x3);
          mpfr_set_ui_2exp (y1, 1, py - 1, GMP_RNDN);
          mpfr_get_f (x1, y1, GMP_RNDN);  /* exact (power of 2) */
          mpf_set (x2, x1);
          mpfr_set (y2, y1, GMP_RNDN);

          for (e = py - 2; e >= 0; e--)
            {
              int inex;
              mpf_div_2exp (x2, x2, 1);
              mpf_add (x1, x1, x2);
              mpfr_div_2exp (y2, y2, 1, GMP_RNDN);
              inex = mpfr_add (y1, y1, y2, GMP_RNDN);
              MPFR_ASSERTN (inex == 0);
              mpfr_set_f (y3, x1, GMP_RNDN);
              if (! mpfr_equal_p (y1, y3))
                break;
              mpfr_get_f (x3, y3, GMP_RNDN);
              if (mpf_cmp (x1, x3) != 0)
                {
                  printf ("Error in prec_test (px = %d, py = %d, e = %d)\n",
                          px, py, e);
                  printf ("x1 = ");
                  mpf_out_str (stdout, 16, 0, x1);
                  printf ("\nx2 = ");
                  mpf_out_str (stdout, 16, 0, x1);
                  printf ("\n");
                  exit (1);
                }
            }

          mpf_clear (x1);
          mpf_clear (x2);
          mpf_clear (x3);
        }

      mpfr_clear (y1);
      mpfr_clear (y2);
      mpfr_clear (y3);
    }
}

int
main (void)
{
  mpf_t x;
  mpfr_t y, z;
  unsigned long i;
  mp_exp_t e;
  int inex;

  tests_start_mpfr ();

  mpfr_init (y);
  mpfr_init (z);
  mpf_init (x);

  mpfr_set_nan (y);
  if (mpfr_get_f (x, y, GMP_RNDN) == 0)
    {
      printf ("Error: mpfr_get_f(NaN) should fail\n");
      exit (1);
    }

  mpfr_set_inf (y, 1);
  if (mpfr_get_f (x, y, GMP_RNDN) == 0)
    {
      printf ("Error: mpfr_get_f(+Inf) should fail\n");
      exit (1);
    }

  mpfr_set_inf (y, -1);
  if (mpfr_get_f (x, y, GMP_RNDN) == 0)
    {
      printf ("Error: mpfr_get_f(-Inf) should fail\n");
      exit (1);
    }

  mpfr_set_ui (y, 0, GMP_RNDN);
  if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_ui (x, 0))
    {
      printf ("Error: mpfr_get_f(+0) fails\n");
      exit (1);
    }

  mpfr_set_ui (y, 0, GMP_RNDN);
  mpfr_neg (y, y, GMP_RNDN);
  if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_ui (x, 0))
    {
      printf ("Error: mpfr_get_f(-0) fails\n");
      exit (1);
    }

  i = 1;
  while (i)
    {
      mpfr_set_ui (y, i, GMP_RNDN);
      if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_ui (x, i))
        {
          printf ("Error: mpfr_get_f(%lu) fails\n", i);
          exit (1);
        }
      if (i <= - (unsigned long) LONG_MIN)
        {
          long j = i < - (unsigned long) LONG_MIN ? - (long) i : LONG_MIN;
          mpfr_set_si (y, j, GMP_RNDN);
          if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_si (x, j))
            {
              printf ("Error: mpfr_get_f(-%lu) fails\n", i);
              exit (1);
            }
        }
      i *= 2;
    }

  /* same tests, but with a larger precision for y, which requires to
     round it */
  mpfr_set_prec (y, 100);
  i = 1;
  while (i)
    {
      mpfr_set_ui (y, i, GMP_RNDN);
      if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_ui (x, i))
        {
          printf ("Error: mpfr_get_f(%lu) fails\n", i);
          exit (1);
        }
      mpfr_set_si (y, (signed long) -i, GMP_RNDN);
      if (mpfr_get_f (x, y, GMP_RNDN) || mpf_cmp_si (x, (signed long) -i))
        {
          printf ("Error: mpfr_get_f(-%lu) fails\n", i);
          exit (1);
        }
      i *= 2;
    }

  /* bug reported by Jim White */
  for (e = 0; e <= 2 * BITS_PER_MP_LIMB; e++)
    {
      /* test with 2^(-e) */
      mpfr_set_ui (y, 1, GMP_RNDN);
      mpfr_div_2exp (y, y, e, GMP_RNDN);
      mpfr_get_f (x, y, GMP_RNDN);
      mpf_mul_2exp (x, x, e);
      if (mpf_cmp_ui (x, 1) != 0)
        {
          printf ("Error: mpfr_get_f(x,y,GMP_RNDN) fails\n");
          printf ("y=");
          mpfr_dump (y);
          printf ("x=");
          mpf_div_2exp (x, x, e);
          mpf_dump (x);
          exit (1);
        }

      /* test with 2^(e) */
      mpfr_set_ui (y, 1, GMP_RNDN);
      mpfr_mul_2exp (y, y, e, GMP_RNDN);
      mpfr_get_f (x, y, GMP_RNDN);
      mpf_div_2exp (x, x, e);
      if (mpf_cmp_ui (x, 1) != 0)
        {
          printf ("Error: mpfr_get_f(x,y,GMP_RNDN) fails\n");
          printf ("y=");
          mpfr_dump (y);
          printf ("x=");
          mpf_mul_2exp (x, x, e);
          mpf_dump (x);
          exit (1);
        }
    }

  /* Bug reported by Yury Lukach on 2006-04-05 */
  mpfr_set_prec (y, 32);
  mpfr_set_prec (z, 32);
  mpf_set_prec (x, 32);
  mpfr_set_ui_2exp (y, 0xc1234567, -30, GMP_RNDN);
  mpfr_get_f (x, y, GMP_RNDN);
  inex = mpfr_set_f (z, x, GMP_RNDN);
  if (inex || ! mpfr_equal_p (y, z))
    {
      printf ("Error in mpfr_get_f:\n  inex = %d, y = ", inex);
      mpfr_dump (z);
      printf ("Expected:\n  inex = 0, y = ");
      mpfr_dump (y);
      exit (1);
    }

  mpfr_clear (y);
  mpfr_clear (z);
  mpf_clear (x);

  prec_test ();

  tests_end_mpfr ();
  return 0;
}
