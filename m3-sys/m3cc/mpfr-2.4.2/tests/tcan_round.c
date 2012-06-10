/* Test file for mpfr_can_round.

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

#define MAX_LIMB_SIZE 100

static void
check_round_p (void)
{
  mp_limb_t buf[MAX_LIMB_SIZE];
  mp_size_t n, i;
  mp_prec_t p;
  mp_exp_t err;
  int r1, r2;

  for (n = 2 ; n <= MAX_LIMB_SIZE ; n++)
    {
      /* avoid mpn_random which leaks memory */
      for (i = 0; i < n; i++)
        buf[i] = randlimb ();
      p = (mp_prec_t) randlimb() % ((n-1) * BITS_PER_MP_LIMB) + MPFR_PREC_MIN;
      err = p + randlimb () % BITS_PER_MP_LIMB;
      r1 = mpfr_round_p (buf, n, err, p);
      r2 = mpfr_can_round_raw (buf, n, MPFR_SIGN_POS, err,
                               GMP_RNDN, GMP_RNDZ, p);
      if (r1 != r2)
        {
          printf ("mpfr_round_p(%d) != mpfr_can_round(%d)!\n"
                  "bn = %ld, err0 = %ld, prec = %lu\nbp = ",
                  r1, r2, n, err, (unsigned long) p);
          gmp_printf ("%NX\n", buf, n);
          exit (1);
        }
    }
}

int
main (void)
{
  mpfr_t x;
  mp_prec_t i, j;

  tests_start_mpfr ();

  /* checks that rounds to nearest sets the last
     bit to zero in case of equal distance */
  mpfr_init2 (x, 59);
  mpfr_set_str_binary (x, "-0.10010001010111000011110010111010111110000000111101100111111E663");
  if (mpfr_can_round (x, 54, GMP_RNDZ, GMP_RNDZ, 53) != 0)
    {
      printf ("Error (1) in mpfr_can_round\n");
      exit (1);
    }

  mpfr_set_str_binary (x, "-Inf");
  if (mpfr_can_round (x, 2000, GMP_RNDZ, GMP_RNDZ, 2000) != 0)
    {
      printf ("Error (2) in mpfr_can_round\n");
      exit (1);
    }

  mpfr_set_prec (x, 64);
  mpfr_set_str_binary (x, "0.1011001000011110000110000110001111101011000010001110011000000000");
  if (mpfr_can_round (x, 65, GMP_RNDN, GMP_RNDN, 54))
    {
      printf ("Error (3) in mpfr_can_round\n");
      exit (1);
    }

  mpfr_set_prec (x, 62);
  mpfr_set_str (x, "0.ff4ca619c76ba69", 16, GMP_RNDZ);
  for (i = 30; i < 99; i++)
    for (j = 30; j < 99; j++)
      {
        int r1, r2;
        for (r1 = 0; r1 < GMP_RND_MAX ; r1++)
          for (r2 = 0; r2 < GMP_RND_MAX ; r2++)
            mpfr_can_round (x, i, (mp_rnd_t) r1, (mp_rnd_t) r2, j); /* test for assertions */
      }

  mpfr_clear (x);

  check_round_p ();

  tests_end_mpfr ();
  return 0;
}
