/* Test file for mpfr_erf and mpfr_erfc.

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
Contributed by Ludovic Meunier and Paul Zimmermann.

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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

#define TEST_FUNCTION mpfr_erf
#define test_generic test_generic_erf
#include "tgeneric.c"

#define TEST_FUNCTION mpfr_erfc
#undef TEST_RANDOM_EMAX
#define TEST_RANDOM_EMAX 63
#define test_generic test_generic_erfc
#include "tgeneric.c"

static void
special_erf (void)
{
  mpfr_t x, y;
  int inex;

  mpfr_init2 (x, 53);
  mpfr_init2 (y, 53);

  /* erf(NaN) = NaN */
  mpfr_set_nan (x);
  mpfr_erf (y, x, GMP_RNDN);
  if (!mpfr_nan_p (y))
    {
      printf ("mpfr_erf failed for x=NaN\n");
      exit (1);
    }

  /* erf(+Inf) = 1 */
  mpfr_set_inf (x, 1);
  mpfr_erf (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 1))
    {
      printf ("mpfr_erf failed for x=+Inf\n");
      printf ("expected 1.0, got ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  /* erf(-Inf) = -1 */
  mpfr_set_inf (x, -1);
  mpfr_erf (y, x, GMP_RNDN);
  if (mpfr_cmp_si (y, -1))
    {
      printf ("mpfr_erf failed for x=-Inf\n");
      exit (1);
    }

  /* erf(+0) = +0 */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_erf (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 0) || mpfr_sgn (y) < 0)
    {
      printf ("mpfr_erf failed for x=+0\n");
      exit (1);
    }

  /* erf(-0) = -0 */
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_erf (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 0) || mpfr_sgn (y) > 0)
    {
      printf ("mpfr_erf failed for x=-0\n");
      exit (1);
    }

  mpfr_set_ui (x, 1, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDN);
  mpfr_set_str_binary (y, "0.11010111101110110011110100111010000010000100010001011");
  if (mpfr_cmp (x, y))
    {
      printf ("mpfr_erf failed for x=1.0, rnd=GMP_RNDN\n");
      printf ("expected ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_str (x, "6.6", 10, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("mpfr_erf failed for x=6.6, rnd=GMP_RNDN\n");
      printf ("expected 1\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_str (x, "-6.6", 10, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp_si (x, -1))
    {
      printf ("mpfr_erf failed for x=-6.6, rnd=GMP_RNDN\n");
      printf ("expected -1\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_str (x, "6.6", 10, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDZ);
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111111111111111111111111");
  if (mpfr_cmp (x, y))
    {
      printf ("mpfr_erf failed for x=6.6, rnd=GMP_RNDZ\n");
      printf ("expected ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_str (x, "4.5", 10, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDN);
  mpfr_set_str_binary (y, "0.1111111111111111111111111111111100100111110100011");
  if (mpfr_cmp (x, y))
    {
      printf ("mpfr_erf failed for x=4.5, rnd=GMP_RNDN\n");
      printf ("expected ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 120);
  mpfr_set_prec (y, 120);
  mpfr_set_str_binary (x, "0.110100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011E3");
  mpfr_erf (x, x, GMP_RNDN);
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111111111111111111111111111111111111100111111000100111011111011010000110101111100011001101");
  if (mpfr_cmp (x, y))
    {
      printf ("mpfr_erf failed for x=6.6, rnd=GMP_RNDN\n");
      printf ("expected ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 8);
  mpfr_set_prec (y, 8);
  mpfr_set_ui (x, 50, GMP_RNDN);
  inex = mpfr_erf (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 1))
    {
      printf ("mpfr_erf failed for x=50, rnd=GMP_RNDN\n");
      printf ("expected 1, got ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      exit (1);
    }
  if (inex <= 0)
    {
      printf ("mpfr_erf failed for x=50, rnd=GMP_RNDN: wrong ternary value\n"
              "expected positive, got %d\n", inex);
      exit (1);
    }
  inex = mpfr_erf (x, x, GMP_RNDZ);
  mpfr_nextbelow (y);
  if (mpfr_cmp (x, y))
    {
      printf ("mpfr_erf failed for x=50, rnd=GMP_RNDZ\n");
      printf ("expected ");
      mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
      printf ("\n");
      printf ("got      ");
      mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }
  if (inex >= 0)
    {
      printf ("mpfr_erf failed for x=50, rnd=GMP_RNDN: wrong ternary value\n"
              "expected negative, got %d\n", inex);
      exit (1);
    }

  mpfr_set_prec (x, 32);
  mpfr_set_prec (y, 32);

  mpfr_set_str_binary (x, "0.1010100100111011001111100101E-1");
  mpfr_set_str_binary (y, "0.10111000001110011010110001101011E-1");
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (1)\n");
      exit (1);
    }

  mpfr_set_str_binary (x, "-0.10110011011010111110010001100001");
  mpfr_set_str_binary (y, "-0.1010110110101011100010111000111");
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (2)\n");
      mpfr_print_binary (x); printf ("\n");
      exit (1);
    }

  mpfr_set_str_binary (x, "100.10001110011110100000110000111");
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111");
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (3)\n");
      exit (1);
    }
  mpfr_set_str_binary (x, "100.10001110011110100000110000111");
  mpfr_erf (x, x, GMP_RNDZ);
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (4)\n");
      exit (1);
    }
  mpfr_set_str_binary (x, "100.10001110011110100000110000111");
  mpfr_erf (x, x, GMP_RNDU);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("Error: erf for prec=32 (5)\n");
      exit (1);
    }

  mpfr_set_str_binary (x, "100.10001110011110100000110001000");
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("Error: erf for prec=32 (6)\n");
      exit (1);
    }
  mpfr_set_str_binary (x, "100.10001110011110100000110001000");
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111");
  mpfr_erf (x, x, GMP_RNDZ);
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (7)\n");
      exit (1);
    }
  mpfr_set_str_binary (x, "100.10001110011110100000110001000");
  mpfr_erf (x, x, GMP_RNDU);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("Error: erf for prec=32 (8)\n");
      exit (1);
    }

  mpfr_set_ui (x, 5, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDN);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("Error: erf for prec=32 (9)\n");
      exit (1);
    }
  mpfr_set_ui (x, 5, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDU);
  if (mpfr_cmp_ui (x, 1))
    {
      printf ("Error: erf for prec=32 (10)\n");
      exit (1);
    }
  mpfr_set_ui (x, 5, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDZ);
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (11)\n");
      exit (1);
    }
  mpfr_set_ui (x, 5, GMP_RNDN);
  mpfr_erf (x, x, GMP_RNDD);
  mpfr_set_str_binary (y, "0.11111111111111111111111111111111");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=32 (12)\n");
      exit (1);
    }

  mpfr_set_prec (x, 43);
  mpfr_set_prec (y, 64);
  mpfr_set_str_binary (x, "-0.1101110110101111100101011101110101101001001e3");
  mpfr_erf (y, x, GMP_RNDU);
  mpfr_set_prec (x, 64);
  mpfr_set_str_binary (x, "-0.1111111111111111111111111111111111111111111111111111111111111111");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for prec=43,64 (13)\n");
      exit (1);
    }

  /* worst cases */
  mpfr_set_prec (x, 53);
  mpfr_set_prec (y, 53);
  mpfr_set_str_binary (x, "1.0000000000000000000000000000000000000110000000101101");
  mpfr_erf (y, x, GMP_RNDN);
  mpfr_set_str_binary (x, "0.110101111011101100111101001110100000101011000011001");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for worst case (1)\n");
      exit (1);
    }

  mpfr_set_str_binary (x, "1.0000000000000000000000000000011000111010101101011010");
  mpfr_erf (y, x, GMP_RNDU);
  mpfr_set_str_binary (x, "0.11010111101110110011110100111100100111100011111000110");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for worst case (2a)\n");
      exit (1);
    }
  mpfr_set_str_binary (x, "1.0000000000000000000000000000011000111010101101011010");
  mpfr_erf (y, x, GMP_RNDD);
  mpfr_set_str_binary (x, "0.11010111101110110011110100111100100111100011111000101");
  if (mpfr_cmp (x, y))
    {
      printf ("Error: erf for worst case (2b)\n");
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
}

static void
special_erfc (void)
{
  mpfr_t x, y;

  mpfr_inits (x, y, (mpfr_ptr) 0);

  /* erfc (NaN) = NaN */
  mpfr_set_nan (x);
  mpfr_erfc (y, x, GMP_RNDN);
  if (!mpfr_nan_p (y))
    {
      printf ("mpfr_erfc failed for x=NaN\n");
      exit (1);
    }
  /* erfc(+Inf) = 0+ */
  mpfr_set_inf (x, 1);
  mpfr_erfc (y, x, GMP_RNDN);
  if (!MPFR_IS_ZERO (y) || !MPFR_IS_POS (y))
    {
      printf ("mpfr_erf failed for x=+Inf\n");
      printf ("expected 0+, got ");
      mpfr_dump (y);
      exit (1);
    }
  /* erfc(-Inf) = 2 */
  mpfr_set_inf (x, -1);
  mpfr_erfc (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 2))
    {
      printf ("mpfr_erf failed for x=-Inf\n");
      printf ("expected 2, got ");
      mpfr_dump (y);
      exit (1);
    }
  /* erf(+0) = 1 */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_erfc (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 1))
    {
      printf ("mpfr_erf failed for x=+0\n");
      printf ("expected 1, got ");
      mpfr_dump (y);
      exit (1);
    }

  mpfr_clears (x, y, (mpfr_ptr) 0);
}

static void
large_arg (void)
{
  mpfr_t x, y;

  mpfr_init2 (x, 88);
  mpfr_init2 (y, 98);

  mpfr_set_si_2exp (x, -1, 173, GMP_RNDN);
  mpfr_erfc (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 2) != 0)
    {
      printf ("mpfr_erfc failed for large x (1)\n");
      exit (1);
    }


  mpfr_set_prec (x, 33);
  mpfr_set_prec (y, 43);
  mpfr_set_str_binary (x, "1.11000101010111011000111100101001e6");
  mpfr_erfc (y, x, GMP_RNDD);
  mpfr_set_prec (x, 43);
  mpfr_set_str_binary (x, "100010011100101100001101100101011101101E-18579");
  if (mpfr_cmp (x, y) != 0)
    {
      printf ("mpfr_erfc failed for large x (2)\n");
      exit (1);
    }

  mpfr_set_prec (y, 43);
  mpfr_set_si_2exp (x, 1, 11, GMP_RNDN);
  mpfr_erfc (y, x, GMP_RNDN);
  mpfr_set_str_binary (x, "0.1100000100100010101111001111010010001000110E-6051113");
  if (mpfr_cmp (x, y) != 0)
    {
      printf ("mpfr_erfc failed for large x (3)\n");
      exit (1);
    }

  mpfr_set_prec (x, 75);
  mpfr_set_prec (y, 85);
  mpfr_set_str_binary (x, "0.111110111111010011101011001100001010011110101010011111010010111101010001011E15");
  mpfr_erfc (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 0) || mpfr_sgn (y) < 0)
    {
      printf ("mpfr_erfc failed for large x (3b)\n");
      exit (1);
    }

  mpfr_set_prec (x, 2);
  mpfr_set_prec (y, 21);
  mpfr_set_str_binary (x, "-1.0e3");
  mpfr_erfc (y, x, GMP_RNDZ);
  mpfr_set_prec (x, 21);
  mpfr_set_str_binary (x, "1.11111111111111111111");
  if (mpfr_cmp (x, y) != 0)
    {
      printf ("mpfr_erfc failed for large x (4)\n");
      exit (1);
    }

  mpfr_set_prec (x, 2);
  mpfr_set_prec (y, 31);
  mpfr_set_str_binary (x, "-1.0e3");
  mpfr_erfc (y, x, GMP_RNDZ);
  mpfr_set_prec (x, 31);
  mpfr_set_str_binary (x, "1.111111111111111111111111111111");
  if (mpfr_cmp (x, y) != 0)
    {
      printf ("mpfr_erfc failed for x=-8, prec=31 (5)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  /* Reported by Christopher Creutzig on 2007-07-10. */
  mpfr_set_prec (x, 53);
  mpfr_set_prec (y, 53);
  mpfr_set_si_2exp (x, 54563, -1, GMP_RNDN);
  mpfr_erfc (y, x, GMP_RNDZ);
  mpfr_set_ui (x, 0, GMP_RNDN);
  if (! mpfr_equal_p (y, x))
    {
      printf ("mpfr_erfc failed for x=27281.5, prec=53 (6)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  /* same test with rounding away from zero */
  mpfr_set_si_2exp (x, 54563, -1, GMP_RNDN);
  mpfr_erfc (y, x, GMP_RNDU);
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_nextabove (x);
  if (! mpfr_equal_p (y, x))
    {
      printf ("mpfr_erfc failed for x=27281.5, prec=53 (7)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
}

static void
test_erfc (void)
{
  mpfr_t x, y, z;
  int inex;

  mpfr_inits2 (40, x, y, z, (mpfr_ptr) 0);

  mpfr_set_si_2exp (x, -1, -10, GMP_RNDN);
  mpfr_set_str_binary (z, "0.1000000000100100000110111010110111100000E1");
  mpfr_erfc (y, x, GMP_RNDN);
  if (mpfr_cmp (y, z) != 0)
    {
      printf ("mpfr_erfc failed for x = ");
      mpfr_dump (x);
      printf ("got        ");
      mpfr_dump (y);
      printf ("instead of ");
      mpfr_dump (z);
      exit (1);
    }

  /* slowness detected by Kevin Rauch on 26 Oct 2007 */
  mpfr_set_prec (x, 128);
  mpfr_set_si (x, -256, GMP_RNDN);
  inex = mpfr_erfc (x, x, GMP_RNDN);
  MPFR_ASSERTN(inex > 0 && mpfr_cmp_ui (x, 2) == 0);

  mpfr_clears (x, y, z, (mpfr_ptr) 0);
}

int
main (int argc, char *argv[])
{
  tests_start_mpfr ();

  special_erf ();
  special_erfc ();
  large_arg ();
  test_erfc ();

  test_generic_erf (2, 100, 15);
  test_generic_erfc (2, 100, 15);

  data_check ("data/erf",  mpfr_erf,  "mpfr_erf");
  data_check ("data/erfc", mpfr_erfc, "mpfr_erfc");

  tests_end_mpfr ();
  return 0;
}
