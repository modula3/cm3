/* mpfr_pow_z -- power function x^z with z a MPZ

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

#define MPFR_NEED_LONGLONG_H
#include "mpfr-impl.h"

static int
mpfr_pow_pos_z (mpfr_ptr y, mpfr_srcptr x, mpz_srcptr z, mp_rnd_t rnd)
{
  mpfr_t res;
  mp_prec_t prec, err;
  int inexact;
  mp_rnd_t rnd1;
  mpz_t absz;
  mp_size_t size_z;
  MPFR_ZIV_DECL (loop);

  MPFR_ASSERTD (mpz_sgn (z) != 0);

  if (MPFR_UNLIKELY (mpz_cmpabs_ui (z, 1) == 0))
    return mpfr_set (y, x, rnd);

  rnd1 = MPFR_IS_POS (x) ? GMP_RNDU : GMP_RNDD; /* away */
  absz[0] = z[0];
  SIZ (absz) = ABS(SIZ(absz)); /* Hack to get abs(z) */
  MPFR_MPZ_SIZEINBASE2 (size_z, z);

  prec = MPFR_PREC (y) + 3 + size_z + MPFR_INT_CEIL_LOG2 (MPFR_PREC (y));
  mpfr_init2 (res, prec);

  MPFR_ZIV_INIT (loop, prec);
  for (;;)
    {
      mp_size_t i = size_z;
      /* now 2^(i-1) <= z < 2^i */
      /* see pow_ui.c for the error analusis, which is identical */
      MPFR_ASSERTD (prec > (mpfr_prec_t) i);
      err = prec - 1 - (mpfr_prec_t) i;
      MPFR_ASSERTD (i >= 2);
      mpfr_clear_overflow ();
      mpfr_clear_underflow ();

      /* First step: compute square from y */
      inexact = mpfr_mul (res, x, x, GMP_RNDU);
      if (mpz_tstbit (absz, i-2))
        inexact |= mpfr_mul (res, res, x, rnd1);
      for (i -= 3; i >= 0 && !mpfr_underflow_p() && !mpfr_overflow_p (); i--)
        {
          inexact |= mpfr_mul (res, res, res, GMP_RNDU);
          if (mpz_tstbit (absz, i))
            inexact |= mpfr_mul (res, res, x, rnd1);
        }
      /*    printf ("pow_z ");
            mpfr_dump_mant (MPFR_MANT (res), prec, MPFR_PREC (x), err); */
      if (MPFR_LIKELY (inexact == 0
                       || mpfr_overflow_p () || mpfr_underflow_p ()
                       || MPFR_CAN_ROUND (res, err, MPFR_PREC (y), rnd)))
        break;
      /* Actualisation of the precision */
      MPFR_ZIV_NEXT (loop, prec);
      mpfr_set_prec (res, prec);
    }
  MPFR_ZIV_FREE (loop);

  inexact = mpfr_set (y, res, rnd);
  mpfr_clear (res);

  /* Check Overflow */
  if (MPFR_UNLIKELY (mpfr_overflow_p ()))
    return mpfr_overflow (y, rnd,
              mpz_odd_p (absz) ? MPFR_SIGN (x) : MPFR_SIGN_POS);
  /* Check Underflow */
  else if (MPFR_UNLIKELY (mpfr_underflow_p ()))
    {
      if (rnd == GMP_RNDN)
        rnd = GMP_RNDZ;
      return mpfr_underflow (y, rnd,
         mpz_odd_p (absz) ? MPFR_SIGN (x) : MPFR_SIGN_POS);
    }
  return inexact;
}

/* The computation of y=pow(x,z) is done by
 *    y=pow_ui(x,z) if z>0
 * else
 *    y=1/pow_ui(x,z) if z<0
 */

int
mpfr_pow_z (mpfr_ptr y, mpfr_srcptr x, mpz_srcptr z, mp_rnd_t rnd)
{
  int   inexact;
  mpz_t tmp;
  MPFR_SAVE_EXPO_DECL (expo);

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN (x))
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
      else if (mpz_sgn (z) == 0)  /* y^0 = 1 for any y except NAN */
        return mpfr_set_ui (y, 1, rnd);
      else if (MPFR_IS_INF (x))
        {
          /* Inf^n = Inf, (-Inf)^n = Inf for n even, -Inf for n odd */
          /* Inf ^(-n) = 0, sign = + if x>0 or z even */
          if (mpz_sgn (z) > 0)
            MPFR_SET_INF (y);
          else
            MPFR_SET_ZERO (y);
          if (MPFR_UNLIKELY (MPFR_IS_NEG (x) && mpz_odd_p (z)))
            MPFR_SET_NEG (y);
          else
            MPFR_SET_POS (y);
          MPFR_RET (0);
        }
      else /* x is zero */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO(x));
          if (mpz_sgn (z) > 0)
            /* 0^n = +/-0 for any n */
            MPFR_SET_ZERO (y);
          else
            /* 0^(-n) if +/- INF */
            MPFR_SET_INF (y);
          if (MPFR_LIKELY (MPFR_IS_POS (x) || mpz_even_p (z)))
            MPFR_SET_POS (y);
          else
            MPFR_SET_NEG (y);
          MPFR_RET(0);
        }
    }

  if (MPFR_UNLIKELY (mpz_sgn (z) == 0))
    /* y^0 = 1 for any y except NAN */
    return mpfr_set_ui (y, 1, rnd);

  /* detect exact powers: x^-n is exact iff x is a power of 2
     Do it if n > 0 too (faster). */
  if (MPFR_UNLIKELY (mpfr_cmp_si_2exp (x, MPFR_SIGN (x),
                                       MPFR_EXP (x) - 1) == 0))
    {
      mp_exp_t expx = MPFR_EXP (x); /* warning: x and y may be the same
                                       variable */
      mpfr_set_si (y, mpz_odd_p (z) ? MPFR_INT_SIGN(x) : 1, rnd);
      MPFR_ASSERTD (MPFR_IS_FP (y));
      mpz_init (tmp);
      mpz_mul_si (tmp, z, expx-1);
      MPFR_ASSERTD (MPFR_GET_EXP (y) == 1);
      mpz_add_ui (tmp, tmp, 1);
      inexact = 0;
      if (MPFR_UNLIKELY (mpz_cmp_si (tmp, __gmpfr_emin) < 0))
        {
          /* The following test is necessary because in the rounding to the
           * nearest mode, mpfr_underflow always rounds away from 0. In
           * this rounding mode, we need to round to 0 if:
           *   _ |y| < 2^(emin-2), or
           *   _ |y| = 2^(emin-2) and the absolute value of the exact
           *     result is <= 2^(emin-2).
           * NOTE: y is a power of 2 and inexact = 0!
           */
          if (rnd == GMP_RNDN && mpz_cmp_si (tmp, __gmpfr_emin-1) < 0)
            rnd = GMP_RNDZ;
          inexact = mpfr_underflow (y, rnd, MPFR_SIGN (y));
        }
      else if (MPFR_UNLIKELY (mpz_cmp_si (tmp, __gmpfr_emax) > 0))
        inexact = mpfr_overflow (y, rnd, MPFR_SIGN(x));
      else
        MPFR_SET_EXP (y, mpz_get_si (tmp));
      mpz_clear (tmp);
      MPFR_RET (inexact);
    }

  MPFR_SAVE_EXPO_MARK (expo);
  __gmpfr_emin -= 3; /* So that we can check for underflow properly */

  if (mpz_sgn (z) > 0)
    inexact = mpfr_pow_pos_z (y, x, z, rnd);
  else
    {
      /* Declaration of the intermediary variable */
      mpfr_t t;
      mp_prec_t Nt;   /* Precision of the intermediary variable */
      MPFR_ZIV_DECL (loop);

      /* compute the precision of intermediary variable */
      Nt = MAX (MPFR_PREC (x), MPFR_PREC (y));

      /* the optimal number of bits : see algorithms.ps */
      Nt = Nt + 3 + MPFR_INT_CEIL_LOG2 (Nt);

      /* initialise of intermediary     variable */
      mpfr_init2 (t, Nt);

      MPFR_ZIV_INIT (loop, Nt);
      for (;;)
        {
          /* compute 1/(x^n) n>0 */
          mpfr_pow_pos_z (t, x, z, GMP_RNDN);
          mpfr_ui_div (t, 1, t, GMP_RNDN);
          /* FIXME: old code improved, but I think this is still incorrect. */
          if (MPFR_UNLIKELY (MPFR_IS_ZERO (t)))
            {
              MPFR_ZIV_FREE (loop);
              mpfr_clear (t);
              MPFR_SAVE_EXPO_FREE (expo);
              return mpfr_underflow (y, rnd == GMP_RNDN ? GMP_RNDZ : rnd,
                                     mpz_odd_p (z) ? MPFR_SIGN (x) :
                                     MPFR_SIGN_POS);
            }
          if (MPFR_UNLIKELY (MPFR_IS_INF (t)))
            {
              MPFR_ZIV_FREE (loop);
              mpfr_clear (t);
              MPFR_SAVE_EXPO_FREE (expo);
              return mpfr_overflow (y, rnd,
                                    mpz_odd_p (z) ? MPFR_SIGN (x) :
                                    MPFR_SIGN_POS);
            }
          if (MPFR_LIKELY (MPFR_CAN_ROUND (t, Nt-3, MPFR_PREC (y), rnd)))
            break;
          /* actualisation of the precision */
          MPFR_ZIV_NEXT (loop, Nt);
          mpfr_set_prec (t, Nt);
        }
      MPFR_ZIV_FREE (loop);

      inexact = mpfr_set (y, t, rnd);
      mpfr_clear (t);
    }

  MPFR_SAVE_EXPO_UPDATE_FLAGS (expo, __gmpfr_flags);
  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (y, inexact, rnd);
}
