/* mpfr_sin_cos -- sine and cosine of a floating-point number

Copyright 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

/* (y, z) <- (sin(x), cos(x)), return value is 0 iff both results are exact
   ie, iff x = 0 */
int
mpfr_sin_cos (mpfr_ptr y, mpfr_ptr z, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  mp_prec_t prec, m;
  int neg, reduce;
  mpfr_t c, xr;
  mpfr_srcptr xx;
  mp_exp_t err, expx;
  MPFR_ZIV_DECL (loop);

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN(x) || MPFR_IS_INF(x))
        {
          MPFR_SET_NAN (y);
          MPFR_SET_NAN (z);
          MPFR_RET_NAN;
        }
      else /* x is zero */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          MPFR_SET_ZERO (y);
          MPFR_SET_SAME_SIGN (y, x);
          /* y = 0, thus exact, but z is inexact in case of underflow
             or overflow */
          return mpfr_set_ui (z, 1, rnd_mode);
        }
    }

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                  ("sin[%#R]=%R cos[%#R]=%R", y, y, z, z));

  prec = MAX (MPFR_PREC (y), MPFR_PREC (z));
  m = prec + MPFR_INT_CEIL_LOG2 (prec) + 13;
  expx = MPFR_GET_EXP (x);

  mpfr_init (c);
  mpfr_init (xr);

  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      /* the following is copied from sin.c */
      if (expx >= 2) /* reduce the argument */
        {
          reduce = 1;
          mpfr_set_prec (c, expx + m - 1);
          mpfr_set_prec (xr, m);
          mpfr_const_pi (c, GMP_RNDN);
          mpfr_mul_2ui (c, c, 1, GMP_RNDN);
          mpfr_remainder (xr, x, c, GMP_RNDN);
          mpfr_div_2ui (c, c, 1, GMP_RNDN);
          if (MPFR_SIGN (xr) > 0)
            mpfr_sub (c, c, xr, GMP_RNDZ);
          else
            mpfr_add (c, c, xr, GMP_RNDZ);
          if (MPFR_IS_ZERO(xr) || MPFR_EXP(xr) < (mp_exp_t) 3 - (mp_exp_t) m
              || MPFR_EXP(c) < (mp_exp_t) 3 - (mp_exp_t) m)
            goto next_step;
          xx = xr;
        }
      else /* the input argument is already reduced */
        {
          reduce = 0;
          xx = x;
        }

      neg = MPFR_IS_NEG (xx); /* gives sign of sin(x) */
      mpfr_set_prec (c, m);
      mpfr_cos (c, xx, GMP_RNDZ);
      /* If no argument reduction was performed, the error is at most ulp(c),
         otherwise it is at most ulp(c) + 2^(2-m). Since |c| < 1, we have
         ulp(c) <= 2^(-m), thus the error is bounded by 2^(3-m) in that later
         case. */
      if (reduce == 0)
        err = m;
      else
        err = MPFR_GET_EXP (c) + (mp_exp_t) (m - 3);
      if (!mpfr_can_round (c, err, GMP_RNDN, rnd_mode,
                           MPFR_PREC (z) + (rnd_mode == GMP_RNDN)))
        goto next_step;

      mpfr_set (z, c, rnd_mode);
      mpfr_sqr (c, c, GMP_RNDU);
      mpfr_ui_sub (c, 1, c, GMP_RNDN);
      err = 2 + (- MPFR_GET_EXP (c)) / 2;
      mpfr_sqrt (c, c, GMP_RNDN);
      if (neg)
        MPFR_CHANGE_SIGN (c);

      /* the absolute error on c is at most 2^(err-m), which we must put
         in the form 2^(EXP(c)-err). If there was an argument reduction,
         we need to add 2^(2-m); since err >= 2, the error is bounded by
         2^(err+1-m) in that case. */
      err = MPFR_GET_EXP (c) + (mp_exp_t) m - (err + reduce);
      if (mpfr_can_round (c, err, GMP_RNDN, rnd_mode,
                          MPFR_PREC (y) + (rnd_mode == GMP_RNDN)))
        break;
      /* check for huge cancellation */
      if (err < (mp_exp_t) MPFR_PREC (y))
        m += MPFR_PREC (y) - err;
      /* Check if near 1 */
      if (MPFR_GET_EXP (c) == 1
          && MPFR_MANT (c)[MPFR_LIMB_SIZE (c)-1] == MPFR_LIMB_HIGHBIT)
        m += m;

    next_step:
      MPFR_ZIV_NEXT (loop, m);
      mpfr_set_prec (c, m);
    }
  MPFR_ZIV_FREE (loop);

  mpfr_set (y, c, rnd_mode);

  mpfr_clear (c);
  mpfr_clear (xr);

  MPFR_RET (1); /* Always inexact */
}
