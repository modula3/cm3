/* Test file for mpfr_check.

Copyright 2003, 2004, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include <stdio.h>

#include "mpfr-test.h"

#define ERROR(s) printf("mpfr_check failed " s " Prec=%lu\n", pr), exit(1)

int
main (void)
{
  mpfr_t a;
  mp_limb_t *p, tmp;
  mp_size_t s;
  mpfr_prec_t pr;
  int max;

  tests_start_mpfr ();
  for(pr = MPFR_PREC_MIN ; pr < 500 ; pr++)
    {
      mpfr_init2 (a, pr);
      if (!mpfr_check(a)) ERROR("for init");
      /* Check special cases */
      MPFR_SET_NAN(a);
      if (!mpfr_check(a)) ERROR("for nan");
      MPFR_SET_POS(a);
      MPFR_SET_INF(a);
      if (!mpfr_check(a)) ERROR("for inf");
      MPFR_SET_ZERO(a);
      if (!mpfr_check(a)) ERROR("for zero");
      /* Check var */
      mpfr_set_ui(a, 2, GMP_RNDN);
      if (!mpfr_check(a)) ERROR("for set_ui");
      mpfr_clear_overflow();
      max = 1000; /* Allows max 2^1000 bits for the exponent */
      while ((!mpfr_overflow_p()) && (max>0))
        {
          mpfr_mul(a, a, a, GMP_RNDN);
          if (!mpfr_check(a)) ERROR("for mul");
          max--;
        }
      if (max==0) ERROR("can't reach overflow");
      mpfr_set_ui(a, 2137, GMP_RNDN);
      /* Corrupt a and check for it */
      MPFR_SIGN(a) = 2;
      if (mpfr_check(a))  ERROR("sgn");
      MPFR_SET_POS(a);
      /* Check prec */
      MPFR_PREC(a) = 1;
      if (mpfr_check(a))  ERROR("precmin");
      MPFR_PREC(a) = MPFR_PREC_MAX+1;
      if (mpfr_check(a))  ERROR("precmax");
      MPFR_PREC(a) = pr;
      if (!mpfr_check(a)) ERROR("prec");
      /* Check exponent */
      MPFR_EXP(a) = MPFR_EXP_INVALID;
      if (mpfr_check(a))  ERROR("exp invalid");
      MPFR_EXP(a) = -MPFR_EXP_INVALID;
      if (mpfr_check(a))  ERROR("-exp invalid");
      MPFR_EXP(a) = 0;
      if (!mpfr_check(a)) ERROR("exp 0");
      /* Check Mantissa */
      p = MPFR_MANT(a);
      MPFR_MANT(a) = NULL;
      if (mpfr_check(a))  ERROR("Mantissa Null Ptr");
      MPFR_MANT(a) = p;
      /* Check size */
      s = MPFR_GET_ALLOC_SIZE(a);
      MPFR_SET_ALLOC_SIZE(a, 0);
      if (mpfr_check(a))  ERROR("0 size");
      MPFR_SET_ALLOC_SIZE(a, MP_SIZE_T_MIN);
      if (mpfr_check(a))  ERROR("min size");
      MPFR_SET_ALLOC_SIZE(a, MPFR_LIMB_SIZE(a)-1 );
      if (mpfr_check(a))  ERROR("size < prec");
      MPFR_SET_ALLOC_SIZE(a, s);
      /* Check normal form */
      tmp = MPFR_MANT(a)[0];
      if ((pr % BITS_PER_MP_LIMB) != 0)
        {
          MPFR_MANT(a)[0] = ~0;
          if (mpfr_check(a))  ERROR("last bits non 0");
        }
      MPFR_MANT(a)[0] = tmp;
      MPFR_MANT(a)[MPFR_LIMB_SIZE(a)-1] &= MPFR_LIMB_MASK (BITS_PER_MP_LIMB-1);
      if (mpfr_check(a))  ERROR("last bits non 0");
      /* Final */
      mpfr_set_ui(a, 2137, GMP_RNDN);
      if (!mpfr_check(a)) ERROR("after last set");
      mpfr_clear (a);
      if (mpfr_check(a))  ERROR("after clear");
    }
  tests_end_mpfr ();
  return 0;
}
