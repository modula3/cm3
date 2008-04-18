/* Test file for mpfr_hypot.

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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
#include <limits.h>
#include <stdlib.h>

#include "mpfr-test.h"

#define TEST_FUNCTION mpfr_hypot

static void
special (void)
{
  mpfr_t x, y, z;

  mpfr_init (x);
  mpfr_init (y);
  mpfr_init (z);

  mpfr_set_nan (x);
  mpfr_hypot (z, x, y, GMP_RNDN);
  MPFR_ASSERTN(mpfr_nan_p (z));

  mpfr_set_inf (x, 1);
  mpfr_set_inf (y, -1);
  mpfr_hypot (z, x, y, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (z) && mpfr_sgn (z) > 0);

  mpfr_set_inf (x, -1);
  mpfr_set_nan (y);
  mpfr_hypot (z, x, y, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (z) && mpfr_sgn (z) > 0);

  mpfr_set_nan (x);
  mpfr_set_inf (y, -1);
  mpfr_hypot (z, x, y, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (z) && mpfr_sgn (z) > 0);

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
}

static void
test_large (void)
{
  mpfr_t x, y, z, t;

  mpfr_init (x);
  mpfr_init (y);
  mpfr_init (z);
  mpfr_init (t);

  mpfr_set_ui (x, 21, GMP_RNDN);
  mpfr_set_ui (y, 28, GMP_RNDN);
  mpfr_set_ui (z, 35, GMP_RNDN);

  mpfr_mul_2ui (x, x, MPFR_EMAX_DEFAULT-6, GMP_RNDN);
  mpfr_mul_2ui (y, y, MPFR_EMAX_DEFAULT-6, GMP_RNDN);
  mpfr_mul_2ui (z, z, MPFR_EMAX_DEFAULT-6, GMP_RNDN);

  mpfr_hypot (t, x, y, GMP_RNDN);
  if (mpfr_cmp (z, t))
    {
      printf ("Error in test_large: got\n");
      mpfr_out_str (stdout, 2, 0, t, GMP_RNDN);
      printf ("\ninstead of\n");
      mpfr_out_str (stdout, 2, 0, z, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 53);
  mpfr_set_prec (t, 53);
  mpfr_set_prec (y, 53);
  mpfr_set_str_binary (x, "0.11101100011110000011101000010101010011001101000001100E-1021");
  mpfr_set_str_binary (t, "0.11111001010011000001110110001101011100001000010010100E-1021");
  mpfr_hypot (y, x, t, GMP_RNDN);

  mpfr_set_prec (x, 240);
  mpfr_set_prec (y, 22);
  mpfr_set_prec (t, 2);
  mpfr_set_str_binary (x, "0.100111011010010010110100000100000001100010011100110101101111111101011110111011011101010110100101111000111100010100110000100101011110111011100110100110100101110101101100011000001100000001111101110100100100011011011010110111100110010101000111e-7");
  mpfr_set_str_binary (y, "0.1111000010000011000111e-10");
  mpfr_hypot (t, x, y, GMP_RNDN);
  mpfr_set_str_binary (y, "0.11E-7");
  if (mpfr_cmp (t, y))
    {
      printf ("Error in mpfr_hypot (1)\n");
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
  mpfr_clear (t);
}

static void
test_large_small (void)
{
  mpfr_t x, y, z;
  int inexact;

  mpfr_init2 (x, 3);
  mpfr_init2 (y, 2);
  mpfr_init2 (z, 2);

  mpfr_set_ui_2exp (x, 1, mpfr_get_emax () / 2, GMP_RNDN);
  mpfr_set_ui_2exp (y, 1, -1, GMP_RNDN);
  inexact = mpfr_hypot (z, x, y, GMP_RNDN);
  if (inexact >= 0 || mpfr_cmp (x, z))
    {
      printf ("Error 1 in test_large_small\n");
      exit (1);
    }

  mpfr_mul_ui (x, x, 5, GMP_RNDN);
  inexact = mpfr_hypot (z, x, y, GMP_RNDN);
  if (mpfr_cmp (x, z) >= 0)
    {
      printf ("Error 2 in test_large_small\n");
      printf ("x = ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      printf ("y = ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("z = ");
      mpfr_out_str (stdout, 2, 0, z, GMP_RNDN);
      printf (" (in precision 2) instead of\n    ");
      mpfr_out_str (stdout, 2, 2, x, GMP_RNDU);
      printf ("\n");
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
}

int
main (int argc, char *argv[])
{
  unsigned int prec, err, yprec, n, p0 = 2, p1 = 100, N = 100;
  mp_rnd_t rnd;
  mpfr_t x1, x2, y, z, t;
  int inexact, compare, compare2;

  tests_start_mpfr ();

  special ();

  mpfr_init (x1);
  mpfr_init (x2);
  mpfr_init (y);
  mpfr_init (z);
  mpfr_init (t);

  /* thypot prec - perform one random computation with precision prec */
  if (argc >= 2)
    {
      p0 = p1 = atoi (argv[1]);
      N = 1;
    }

  for (prec = p0; prec <= p1; prec++)
    {
      mpfr_set_prec (x1, prec);
      mpfr_set_prec (x2, prec);
      mpfr_set_prec (z, prec);
      mpfr_set_prec (t, prec);
      yprec = prec + 10;

      for (n=0; n<N; n++)
        {
          mpfr_random(x1);
          mpfr_random(x2);
          if (randlimb () % 2)
            mpfr_neg (x1, x1, GMP_RNDN);
          if (randlimb () % 2)
            mpfr_neg (x2, x2, GMP_RNDN);
          rnd = (mp_rnd_t) RND_RAND ();
          mpfr_set_prec (y, yprec);

          compare =TEST_FUNCTION (y, x1,x2, rnd);
          err = (rnd == GMP_RNDN) ? yprec + 1 : yprec;
          if (mpfr_can_round (y, err, rnd, rnd, prec))
            {
              mpfr_set (t, y, rnd);
              inexact = TEST_FUNCTION (z, x1,x2, rnd);
              if (mpfr_cmp (t, z))
                {
                  printf ("results differ for x1=");
                  mpfr_out_str (stdout, 2, prec, x1, GMP_RNDN);
                  printf ("\n and x2=");
                  mpfr_out_str (stdout, 2, prec, x2, GMP_RNDN);
                  printf (" \n prec=%u rnd_mode=%s\n", prec,
                          mpfr_print_rnd_mode (rnd));
                  printf ("   got ");
                  mpfr_out_str (stdout, 2, prec, z, GMP_RNDN);
                  puts ("");
                  printf ("   expected ");
                  mpfr_out_str (stdout, 2, prec, t, GMP_RNDN);
                  puts ("");
                  printf ("   approximation was ");
                  mpfr_print_binary (y);
                  puts ("");
                  exit (1);
                }
              compare2 = mpfr_cmp (t, y);
              /* if rounding to nearest, cannot know the sign of t - f(x)
                 because of composed rounding: y = o(f(x)) and t = o(y) */
              if ((rnd != GMP_RNDN) && (compare * compare2 >= 0))
                compare = compare + compare2;
              else
                compare = inexact; /* cannot determine sign(t-f(x)) */
              if (((inexact == 0) && (compare != 0)) ||
                  ((inexact > 0) && (compare <= 0)) ||
                  ((inexact < 0) && (compare >= 0)))
                {
                  printf ("Wrong inexact flag for rnd=%s: expected %d, got %d"
                          "\n", mpfr_print_rnd_mode (rnd), compare, inexact);
                  printf ("x1="); mpfr_print_binary (x1); puts ("");
                  printf ("x2="); mpfr_print_binary (x2); puts ("");
                  printf ("t="); mpfr_print_binary (t); puts ("");
                  exit (1);
                }
            }
        }
    }

  mpfr_clear (x1);
  mpfr_clear (x2);
  mpfr_clear (y);
  mpfr_clear (z);
  mpfr_clear (t);

  test_large ();
  test_large_small ();

  tests_end_mpfr ();
  return 0;
}
