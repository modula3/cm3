/* Test file for mpfr_agm.

Copyright 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

#define check(a,b,r) check4(a,b,r,0.0)

static void
check4 (const char *as, const char *bs, mp_rnd_t rnd_mode, const char *res)
{
  mpfr_t ta, tb, tres;

  mpfr_inits2 (53, ta, tb, tres, (mpfr_ptr) 0);

  mpfr_set_str1 (ta, as);
  mpfr_set_str1 (tb, bs);

  mpfr_agm(tres, ta, tb, rnd_mode);

  if (mpfr_cmp_str1 (tres, res))
    {
      printf ("mpfr_agm failed for a=%s, b=%s, rnd_mode=%d\n",as,bs,rnd_mode);
      printf ("expected result is %s, got ",res);
      mpfr_out_str(stdout, 10, 0, tres, GMP_RNDN);
      putchar('\n');
      exit (1);
  }
  mpfr_clears (ta, tb, tres, (mpfr_ptr) 0);
}

static void
check_large (void)
{
  mpfr_t a, b, agm;
  int inex;

  mpfr_init2 (a, 82);
  mpfr_init2 (b, 82);
  mpfr_init2 (agm, 82);

  mpfr_set_ui (a, 1, GMP_RNDN);
  mpfr_set_str_binary (b, "0.1111101100001000000001011000110111101000001011111000100001000101010100011111110010E-39");
  mpfr_agm (agm, a, b, GMP_RNDN);
  mpfr_set_str_binary (a, "0.1110001000111101101010101010101101001010001001001011100101111011110101111001111100E-4");
  if (mpfr_cmp (agm, a))
    {
      printf ("mpfr_agm failed for precision 82\n");
      exit (1);
    }

  /* problem found by Damien Fischer <damien@maths.usyd.edu.au> 4 Aug 2003:
     produced a division by zero exception */
  mpfr_set_prec (a, 268);
  mpfr_set_prec (b, 268);
  mpfr_set_prec (agm, 268);
  mpfr_set_str (a, "703.93543315330225238487276503953366664991725089988315253092140138947103394917006", 10, GMP_RNDN);
  mpfr_set_str (b, "703.93543315330225238487279020523738740563816490895994499256063816906728642622316", 10, GMP_RNDN);
  mpfr_agm (agm, a, b, GMP_RNDN);

  mpfr_set_prec (a, 18);
  mpfr_set_prec (b, 70);
  mpfr_set_prec (agm, 67);
  mpfr_set_str_binary (a, "0.111001111100101000e8");
  mpfr_set_str_binary (b, "0.1101110111100100010100110000010111011011011100110100111001010100100001e10");
  inex = mpfr_agm (agm, a, b, GMP_RNDN);
  mpfr_set_str_binary (b, "0.1111110010011101101100010101011011010010010000001010100011000110011e9");
  if (mpfr_cmp (agm, b))
    {
      printf ("Error in mpfr_agm (1)\n");
      exit (1);
    }
  if (inex >= 0)
    {
      printf ("Wrong flag for mpfr_agm (1)\n");
      exit (1);
    }

  /* test worst case: 9 consecutive ones after the last bit */
  mpfr_set_prec (a, 2);
  mpfr_set_prec (b, 2);
  mpfr_set_ui (a, 1, GMP_RNDN);
  mpfr_set_ui (b, 2, GMP_RNDN);
  mpfr_set_prec (agm, 904);
  mpfr_agm (agm, a, b, GMP_RNDZ);

  mpfr_clear (a);
  mpfr_clear (b);
  mpfr_clear (agm);
}

static void
check_nans (void)
{
  mpfr_t  x, y, m;

  mpfr_init2 (x, 123L);
  mpfr_init2 (y, 123L);
  mpfr_init2 (m, 123L);

  /* agm(1,nan) == nan */
  mpfr_set_ui (x, 1L, GMP_RNDN);
  mpfr_set_nan (y);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (m));

  /* agm(1,+inf) == +inf */
  mpfr_set_ui (x, 1L, GMP_RNDN);
  mpfr_set_inf (y, 1);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (m));
  MPFR_ASSERTN (mpfr_sgn (m) > 0);

  /* agm(+inf,+inf) == +inf */
  mpfr_set_inf (x, 1);
  mpfr_set_inf (y, 1);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (m));
  MPFR_ASSERTN (mpfr_sgn (m) > 0);

  /* agm(-inf,+inf) == nan */
  mpfr_set_inf (x, -1);
  mpfr_set_inf (y, 1);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (m));

  /* agm(+0,+inf) == nan */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_set_inf (y, 1);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (m));

  /* agm(+0,1) == +0 */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_set_ui (y, 1, GMP_RNDN);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (MPFR_IS_ZERO (m) && MPFR_IS_POS(m));

  /* agm(-0,1) == +0 */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_set_ui (y, 1, GMP_RNDN);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (MPFR_IS_ZERO (m) && MPFR_IS_POS(m));

  /* agm(-0,+0) == +0 */
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_set_ui (y, 0, GMP_RNDN);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (MPFR_IS_ZERO (m) && MPFR_IS_POS(m));

  /* agm(1,1) == 1 */
  mpfr_set_ui (x, 1, GMP_RNDN);
  mpfr_set_ui (y, 1, GMP_RNDN);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_cmp_ui (m ,1) == 0);

  /* agm(-1,-2) == NaN */
  mpfr_set_si (x, -1, GMP_RNDN);
  mpfr_set_si (y, -2, GMP_RNDN);
  mpfr_agm (m, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (m));

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (m);
}

#define TEST_FUNCTION mpfr_agm
#define TWO_ARGS
#define TEST_RANDOM_POS 4
#define TEST_RANDOM_POS2 4
#include "tgeneric.c"

int
main (int argc, char* argv[])
{
  tests_start_mpfr ();

  check_nans ();

  check_large ();
  check4 ("2.0", "1.0", GMP_RNDN, "1.45679103104690677029");
  check4 ("6.0", "4.0", GMP_RNDN, "4.94936087247260925182");
  check4 ("62.0", "61.0", GMP_RNDN, "6.14989837188450749750e+01");
  check4 ("0.5", "1.0", GMP_RNDN, "7.28395515523453385143e-01");
  check4 ("1.0", "2.0", GMP_RNDN, "1.45679103104690677029");
  check4 ("234375765.0", "234375000.0", GMP_RNDN, "2.3437538249984395504e8");
  check4 ("8.0", "1.0", GMP_RNDU, "3.615756177597362786");
  check4 ("1.0", "44.0", GMP_RNDU, "1.33658354512981247808e1");
  check4 ("1.0", "3.7252902984619140625e-9", GMP_RNDU,
          "7.55393356971199025907e-02");
  test_generic (2, 300, 17);

  tests_end_mpfr ();
  return 0;
}
