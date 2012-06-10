/* mpfr_tgamma -- test file for gamma function

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the GNU MPFR Library, and was contributed by Mathieu Dutour.

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

#define TEST_FUNCTION mpfr_gamma
#define RAND_FUNCTION(x) mpfr_random2(x, MPFR_LIMB_SIZE (x), 1)
#include "tgeneric.c"

static void
special (void)
{
  mpfr_t x, y;
  int inex;

  mpfr_init (x);
  mpfr_init (y);

  mpfr_set_nan (x);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_nan_p (y))
    {
      printf ("Error for gamma(NaN)\n");
      exit (1);
    }

  mpfr_set_inf (x, -1);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_nan_p (y))
    {
      printf ("Error for gamma(-Inf)\n");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_inf_p (y) || mpfr_sgn (y) < 0)
    {
      printf ("Error for gamma(+Inf)\n");
      exit (1);
    }

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_inf_p (y) || mpfr_sgn (y) < 0)
    {
      printf ("Error for gamma(+0)\n");
      exit (1);
    }

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_inf_p (y) || mpfr_sgn (y) > 0)
    {
      printf ("Error for gamma(-0)\n");
      exit (1);
    }

  mpfr_set_ui (x, 1, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (mpfr_cmp_ui (y, 1))
    {
      printf ("Error for gamma(1)\n");
      exit (1);
    }

  mpfr_set_si (x, -1, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_nan_p (y))
    {
      printf ("Error for gamma(-1)\n");
      exit (1);
    }

  mpfr_set_prec (x, 53);
  mpfr_set_prec (y, 53);

#define CHECK_X1 "1.0762904832837976166"
#define CHECK_Y1 "0.96134843256452096050"

  mpfr_set_str (x, CHECK_X1, 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_str (x, CHECK_Y1, 10, GMP_RNDN);
  if (mpfr_cmp (y, x))
    {
      printf ("mpfr_lngamma("CHECK_X1") is wrong:\n"
              "expected ");
      mpfr_print_binary (x); putchar ('\n');
      printf ("got      ");
      mpfr_print_binary (y); putchar ('\n');
      exit (1);
    }

#define CHECK_X2 "9.23709516716202383435e-01"
#define CHECK_Y2 "1.0502315560291053398"
  mpfr_set_str (x, CHECK_X2, 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_str (x, CHECK_Y2, 10, GMP_RNDN);
  if (mpfr_cmp (y, x))
    {
      printf ("mpfr_lngamma("CHECK_X2") is wrong:\n"
              "expected ");
      mpfr_print_binary (x); putchar ('\n');
      printf ("got      ");
      mpfr_print_binary (y); putchar ('\n');
      exit (1);
    }

  mpfr_set_prec (x, 8);
  mpfr_set_prec (y, 175);
  mpfr_set_ui (x, 33, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDU);
  mpfr_set_prec (x, 175);
  mpfr_set_str_binary (x, "0.110010101011010101101000010101010111000110011101001000101011000001100010111001101001011E118");
  if (mpfr_cmp (x, y))
    {
      printf ("Error in mpfr_gamma (1)\n");
      exit (1);
    }

  mpfr_set_prec (x, 21);
  mpfr_set_prec (y, 8);
  mpfr_set_ui (y, 120, GMP_RNDN);
  mpfr_gamma (x, y, GMP_RNDZ);
  mpfr_set_prec (y, 21);
  mpfr_set_str_binary (y, "0.101111101110100110110E654");
  if (mpfr_cmp (x, y))
    {
      printf ("Error in mpfr_gamma (120)\n");
      printf ("Expected "); mpfr_print_binary (y); puts ("");
      printf ("Got      "); mpfr_print_binary (x); puts ("");
      exit (1);
    }

  mpfr_set_prec (x, 3);
  mpfr_set_prec (y, 206);
  mpfr_set_str_binary (x, "0.110e10");
  inex = mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 206);
  mpfr_set_str_binary (x, "0.110111100001000001101010010001000111000100000100111000010011100011011111001100011110101000111101101100110001001100110100001001111110000101010000100100011100010011101110000001000010001100010000101001111E6250");
  if (mpfr_cmp (x, y))
    {
      printf ("Error in mpfr_gamma (768)\n");
      exit (1);
    }
  if (inex <= 0)
    {
      printf ("Wrong flag for mpfr_gamma (768)\n");
      exit (1);
    }

  /* worst case to exercise retry */
  mpfr_set_prec (x, 1000);
  mpfr_set_prec (y, 869);
  mpfr_const_pi (x, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);

  mpfr_set_prec (x, 4);
  mpfr_set_prec (y, 4);
  mpfr_set_str_binary (x, "-0.1100E-66");
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_str_binary (x, "-0.1011E67");
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma(-0.1100E-66)\n");
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
}

static void
special_overflow (void)
{
  mpfr_t x, y;
  mp_exp_t emin, emax;
  int inex;

  emin = mpfr_get_emin ();
  emax = mpfr_get_emax ();

  set_emin (-125);
  set_emax (128);

  mpfr_init2 (x, 24);
  mpfr_init2 (y, 24);
  mpfr_set_str_binary (x, "0.101100100000000000110100E7");
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_inf_p (y))
    {
      printf ("Overflow error.\n");
      mpfr_dump (y);
      exit (1);
    }

  /* problem mentioned by Kenneth Wilder, 18 Aug 2005 */
  mpfr_set_prec (x, 29);
  mpfr_set_prec (y, 29);
  mpfr_set_str (x, "-200000000.5", 10, GMP_RNDN); /* exact */
  mpfr_gamma (y, x, GMP_RNDN);
  if (!(mpfr_zero_p (y) && MPFR_SIGN (y) < 0))
    {
      printf ("Error for gamma(-200000000.5)\n");
      printf ("expected -0");
      printf ("got      ");
      mpfr_dump (y);
      exit (1);
    }

  mpfr_set_prec (x, 53);
  mpfr_set_prec (y, 53);
  mpfr_set_str (x, "-200000000.1", 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!(mpfr_zero_p (y) && MPFR_SIGN (y) < 0))
    {
      printf ("Error for gamma(-200000000.1), prec=53\n");
      printf ("expected -0");
      printf ("got      ");
      mpfr_dump (y);
      exit (1);
    }

  /* another problem mentioned by Kenneth Wilder, 29 Aug 2005 */
  mpfr_set_prec (x, 333);
  mpfr_set_prec (y, 14);
  mpfr_set_str (x, "-2.0000000000000000000000005", 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 14);
  mpfr_set_str_binary (x, "-11010011110001E66");
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma(-2.0000000000000000000000005)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  /* another tests from Kenneth Wilder, 31 Aug 2005 */
  set_emax (200);
  set_emin (-200);
  mpfr_set_prec (x, 38);
  mpfr_set_prec (y, 54);
  mpfr_set_str_binary (x, "0.11101111011100111101001001010110101001E-166");
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 54);
  mpfr_set_str_binary (x, "0.100010001101100001110110001010111111010000100101011E167");
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma (test 1)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  set_emax (1000);
  set_emin (-2000);
  mpfr_set_prec (x, 38);
  mpfr_set_prec (y, 71);
  mpfr_set_str_binary (x, "10101011011100001111111000010111110010E-1034");
  /* 184083777010*2^(-1034) */
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 71);
  mpfr_set_str_binary (x, "10111111001000011110010001000000000000110011110000000011101011111111100E926");
  /* 1762885132679550982140*2^926 */
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma (test 2)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  mpfr_set_prec (x, 38);
  mpfr_set_prec (y, 88);
  mpfr_set_str_binary (x, "10111100111001010000100001100100100101E-104");
  /* 202824096037*2^(-104) */
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 88);
  mpfr_set_str_binary (x, "1010110101111000111010111100010110101010100110111000001011000111000011101100001101110010E-21");
  /* 209715199999500283894743922*2^(-21) */
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma (test 3)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  mpfr_set_prec (x, 171);
  mpfr_set_prec (y, 38);
  mpfr_set_str (x, "-2993155353253689176481146537402947624254601559176535", 10,
                GMP_RNDN);
  mpfr_div_2exp (x, x, 170, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 38);
  mpfr_set_str (x, "201948391737", 10, GMP_RNDN);
  mpfr_mul_2exp (x, x, 92, GMP_RNDN);
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma (test 5)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  set_emin (-500000);
  mpfr_set_prec (x, 337);
  mpfr_set_prec (y, 38);
  mpfr_set_str (x, "-30000.000000000000000000000000000000000000000000001", 10,
                GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  mpfr_set_prec (x, 38);
  mpfr_set_str (x, "-3.623795987425E-121243", 10, GMP_RNDN);
  if (mpfr_cmp (x, y))
    {
      printf ("Error for gamma (test 7)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  /* was producing infinite loop */
  set_emin (emin);
  mpfr_set_prec (x, 71);
  mpfr_set_prec (y, 71);
  mpfr_set_str (x, "-200000000.1", 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!(mpfr_zero_p (y) && MPFR_SIGN (y) < 0))
    {
      printf ("Error for gamma (test 8)\n");
      printf ("expected "); mpfr_dump (x);
      printf ("got      "); mpfr_dump (y);
      exit (1);
    }

  set_emax (1073741823);
  mpfr_set_prec (x, 29);
  mpfr_set_prec (y, 29);
  mpfr_set_str (x, "423786866", 10, GMP_RNDN);
  mpfr_gamma (y, x, GMP_RNDN);
  if (!mpfr_inf_p (y) || mpfr_sgn (y) < 0)
    {
      printf ("Error for gamma(423786866)\n");
      exit (1);
    }

  /* check exact result */
  mpfr_set_prec (x, 2);
  mpfr_set_ui (x, 3, GMP_RNDN);
  inex = mpfr_gamma (x, x, GMP_RNDN);
  if (inex != 0 || mpfr_cmp_ui (x, 2) != 0)
    {
      printf ("Error for gamma(3)\n");
      exit (1);
    }

  mpfr_set_emax (1024);
  mpfr_set_prec (x, 53);
  mpfr_set_prec (y, 53);
  mpfr_set_str_binary (x, "101010110100110011111010000110001000111100000110101E-43");
  mpfr_gamma (x, x, GMP_RNDU);
  mpfr_set_str_binary (y, "110000011110001000111110110101011110000100001111111E971");
  if (mpfr_cmp (x, y) != 0)
    {
      printf ("Error for gamma(4)\n");
      printf ("expected "); mpfr_dump (y);
      printf ("got      "); mpfr_dump (x);
      exit (1);
    }

  set_emin (emin);
  set_emax (emax);

  /* bug found by Kevin Rauch, 26 Oct 2007 */
  mpfr_set_str (x, "1e19", 10, GMP_RNDN);
  inex = mpfr_gamma (x, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (x) && inex > 0);

  mpfr_clear (y);
  mpfr_clear (x);
}

/* test gamma on some integral values (from Christopher Creutzig). */
static void
gamma_integer (void)
{
  mpz_t n;
  mpfr_t x, y;
  unsigned int i;

  mpz_init (n);
  mpfr_init2 (x, 149);
  mpfr_init2 (y, 149);

  for (i = 0; i < 100; i++)
    {
      mpz_fac_ui (n, i);
      mpfr_set_ui (x, i+1, GMP_RNDN);
      mpfr_gamma (y, x, GMP_RNDN);
      mpfr_set_z (x, n, GMP_RNDN);
      if (!mpfr_equal_p (x, y))
        {
          printf ("Error for gamma(%u)\n", i+1);
          printf ("expected "); mpfr_dump (x);
          printf ("got      "); mpfr_dump (y);
          exit (1);
        }
    }
  mpfr_clear (y);
  mpfr_clear (x);
  mpz_clear (n);
}

/* bug found by Kevin Rauch */
static void
test20071231 (void)
{
  mpfr_t x;
  int inex;
  mp_exp_t emin;

  emin = mpfr_get_emin ();
  mpfr_set_emin (-1000000);

  mpfr_init2 (x, 21);
  mpfr_set_str (x, "-1000001.5", 10, GMP_RNDN);
  inex = mpfr_gamma (x, x, GMP_RNDN);
  MPFR_ASSERTN(MPFR_IS_ZERO(x) && MPFR_IS_POS(x) && inex < 0);
  mpfr_clear (x);

  mpfr_set_emin (emin);

  mpfr_init2 (x, 53);
  mpfr_set_str (x, "-1000000001.5", 10, GMP_RNDN);
  inex = mpfr_gamma (x, x, GMP_RNDN);
  MPFR_ASSERTN(MPFR_IS_ZERO(x) && MPFR_IS_POS(x) && inex < 0);
  mpfr_clear (x);
}

int
main (int argc, char *argv[])
{
  tests_start_mpfr ();

  special ();
  special_overflow ();
  test_generic (2, 100, 2);
  gamma_integer ();
  test20071231 ();

  data_check ("data/gamma", mpfr_gamma, "mpfr_gamma");

  tests_end_mpfr ();
  return 0;
}
