/* Test file for mpfr_{mul,div}_2{ui,si}.

Copyright 1999, 2001, 2002, 2003, 2004, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

static const char * const val[] = {
  "1.0001@100","4.0004000000000@102", "4.0004000000000@97",
  "1.ABF012345@-100","6.afc048d140000@-98","6.afc048d140000@-103",
  "F.FFFFFFFFF@10000","3.fffffffffc000@10003","3.fffffffffc000@9998",
  "1.23456789ABCDEF@42","4.8d159e26af37c@44","4.8d159e26af37c@39",
  "17@42","5.c000000000000@45","5.c000000000000@40",
  "42@-17","1.0800000000000@-13","1.0800000000000@-18"
};

static int
test_mul (int i, int div, mpfr_ptr y, mpfr_srcptr x,
          unsigned long int n, mp_rnd_t r)
{
  return
    i == 0 ? (div ? mpfr_div_2ui : mpfr_mul_2ui) (y, x, n, r) :
    i == 1 ? (div ? mpfr_div_2si : mpfr_mul_2si) (y, x, n, r) :
    i == 2 ? (div ? mpfr_mul_2si : mpfr_div_2si) (y, x, -n, r) :
    (exit (1), 0);
}

static void
underflow (mp_exp_t e)
{
  mpfr_t x, y, z1, z2;
  mp_exp_t emin;
  int i, k;
  int prec;
  int rnd;
  int div;
  int inex1, inex2;
  unsigned int flags1, flags2;

  /* Test mul_2si(x, e - k), div_2si(x, k - e) and div_2ui(x, k - e)
   * with emin = e, x = 1 + i/16, i in { -1, 0, 1 }, and k = 1 to 4,
   * by comparing the result with the one of a simple division.
   */
  emin = mpfr_get_emin ();
  set_emin (e);
  mpfr_inits2 (8, x, y, (mpfr_ptr) 0);
  for (i = 15; i <= 17; i++)
    {
      inex1 = mpfr_set_ui_2exp (x, i, -4, GMP_RNDN);
      MPFR_ASSERTN (inex1 == 0);
      for (prec = 6; prec >= 3; prec -= 3)
        {
          mpfr_inits2 (prec, z1, z2, (mpfr_ptr) 0);
          RND_LOOP (rnd)
            for (k = 1; k <= 4; k++)
              {
                /* The following one is assumed to be correct. */
                inex1 = mpfr_mul_2si (y, x, e, GMP_RNDN);
                MPFR_ASSERTN (inex1 == 0);
                inex1 = mpfr_set_ui (z1, 1 << k, GMP_RNDN);
                MPFR_ASSERTN (inex1 == 0);
                mpfr_clear_flags ();
                /* Do not use mpfr_div_ui to avoid the optimization
                   by mpfr_div_2si. */
                inex1 = mpfr_div (z1, y, z1, (mpfr_rnd_t) rnd);
                flags1 = __gmpfr_flags;

              for (div = 0; div <= 2; div++)
                {
                  mpfr_clear_flags ();
                  inex2 = div == 0 ?
                    mpfr_mul_2si (z2, x, e - k, (mpfr_rnd_t) rnd) : div == 1 ?
                    mpfr_div_2si (z2, x, k - e, (mpfr_rnd_t) rnd) :
                    mpfr_div_2ui (z2, x, k - e, (mpfr_rnd_t) rnd);
                  flags2 = __gmpfr_flags;
                  if (flags1 == flags2 && SAME_SIGN (inex1, inex2) &&
                      mpfr_equal_p (z1, z2))
                    continue;
                  printf ("Error in underflow(");
                  if (e == MPFR_EMIN_MIN)
                    printf ("MPFR_EMIN_MIN");
                  else if (e == emin)
                    printf ("default emin");
                  else
                    printf ("%ld", e);
                  printf (")\nwith %s, x = %d/16, prec = %d, k = %d, mpfr_%s",
                          div == 0 ? "mul_2si" : div == 1 ? "div_2si" :
                          "div_2ui", i, prec, k, mpfr_print_rnd_mode ((mpfr_rnd_t) rnd));
                  printf ("\nExpected ");
                  mpfr_out_str (stdout, 16, 0, z1, GMP_RNDN);
                  printf (", inex = %d, flags = %u\n", SIGN (inex1), flags1);
                  printf ("Got      ");
                  mpfr_out_str (stdout, 16, 0, z2, GMP_RNDN);
                  printf (", inex = %d, flags = %u\n", SIGN (inex2), flags2);
                  exit (1);
                }  /* div */
              }  /* k */
          mpfr_clears (z1, z2, (mpfr_ptr) 0);
        }  /* prec */
    }  /* i */
  mpfr_clears (x, y, (mpfr_ptr) 0);
  set_emin (emin);
}

static void
underflow0 (void)
{
  underflow (-256);
  if (mpfr_get_emin () != MPFR_EMIN_MIN)
    underflow (mpfr_get_emin ());
  underflow (MPFR_EMIN_MIN);
}

int
main (int argc, char *argv[])
{
  mpfr_t w,z;
  unsigned long k;
  int i;

  tests_start_mpfr ();

  mpfr_inits2 (53, w, z, (mpfr_ptr) 0);

  for (i = 0; i < 3; i++)
    {
      mpfr_set_inf (w, 1);
      test_mul (i, 0, w, w, 10, GMP_RNDZ);
      if (!MPFR_IS_INF(w))
        {
          printf ("Result is not Inf (i = %d)\n", i);
          exit (1);
        }

      mpfr_set_nan (w);
      test_mul (i, 0, w, w, 10, GMP_RNDZ);
      if (!MPFR_IS_NAN(w))
        {
          printf ("Result is not NaN (i = %d)\n", i);
          exit (1);
        }

      for (k = 0 ; k < numberof(val) ; k+=3)
        {
          mpfr_set_str (w, val[k], 16, GMP_RNDN);
          test_mul (i, 0, z, w, 10, GMP_RNDZ);
          if (mpfr_cmp_str (z, val[k+1], 16, GMP_RNDN))
            {
              printf ("ERROR for x * 2^n (i = %d) for %s\n", i, val[k]);
              printf ("Expected: %s\n"
                      "Got     : ", val[k+1]);
              mpfr_out_str (stdout, 16, 0, z, GMP_RNDN);
              putchar ('\n');
              exit (1);
            }
          test_mul (i, 1, z, w, 10, GMP_RNDZ);
          if (mpfr_cmp_str (z, val[k+2], 16, GMP_RNDN))
            {
              printf ("ERROR for x / 2^n (i = %d) for %s\n", i, val[k]);
              printf ("Expected: %s\n"
                      "Got     : ", val[k+2]);
              mpfr_out_str (stdout, 16, 0, z, GMP_RNDN);
              putchar ('\n');
              exit (1);
            }
        }

      mpfr_set_inf (w, 1);
      mpfr_nextbelow (w);
      test_mul (i, 0, w, w, 1, GMP_RNDN);
      if (!mpfr_inf_p (w))
        {
          printf ("Overflow error (i = %d)!\n", i);
          exit (1);
        }
      mpfr_set_ui (w, 0, GMP_RNDN);
      mpfr_nextabove (w);
      test_mul (i, 1, w, w, 1, GMP_RNDN);
      if (mpfr_cmp_ui (w, 0))
        {
          printf ("Underflow error (i = %d)!\n", i);
          exit (1);
        }
    }

  if (MPFR_EXP_MAX >= LONG_MAX/2 && MPFR_EXP_MIN <= LONG_MAX/2-LONG_MAX-1)
    {
      unsigned long lmp1 = (unsigned long) LONG_MAX + 1;

      mpfr_set_ui (w, 1, GMP_RNDN);
      mpfr_mul_2ui (w, w, LONG_MAX/2, GMP_RNDZ);
      mpfr_div_2ui (w, w, lmp1, GMP_RNDZ);
      mpfr_mul_2ui (w, w, lmp1 - LONG_MAX/2, GMP_RNDZ);
      if (!mpfr_cmp_ui (w, 1))
        {
          printf ("Underflow LONG_MAX error!\n");
          exit (1);
        }
    }

  mpfr_clears (w, z, (mpfr_ptr) 0);

  underflow0 ();

  tests_end_mpfr ();
  return 0;
}
