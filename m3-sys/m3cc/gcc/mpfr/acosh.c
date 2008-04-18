/* mpfr_acosh -- inverse hyperbolic cosine

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

#define MPFR_NEED_LONGLONG_H
#include "mpfr-impl.h"

/* The computation of acosh is done by   *
 *  acosh= ln(x + sqrt(x^2-1))           */

int
mpfr_acosh (mpfr_ptr y, mpfr_srcptr x , mp_rnd_t rnd_mode)
{
  MPFR_SAVE_EXPO_DECL (expo);
  int inexact;
  int comp;

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                 ("y[%#R]=%R inexact=%d", y, y, inexact));

  /* Deal with special cases */
  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      /* Nan, or zero or -Inf */
      if (MPFR_IS_INF (x) && MPFR_IS_POS (x))
        {
          MPFR_SET_INF (y);
          MPFR_SET_POS (y);
          MPFR_RET (0);
        }
      else /* Nan, or zero or -Inf */
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
    }
  comp = mpfr_cmp_ui (x, 1);
  if (MPFR_UNLIKELY (comp < 0))
    {
      MPFR_SET_NAN (y);
      MPFR_RET_NAN;
    }
  else if (MPFR_UNLIKELY (comp == 0))
    {
      MPFR_SET_ZERO (y); /* acosh(1) = 0 */
      MPFR_SET_POS (y);
      MPFR_RET (0);
    }
  MPFR_SAVE_EXPO_MARK (expo);

  /* General case */
  {
    /* Declaration of the intermediary variables */
    mpfr_t t;
    /* Declaration of the size variables */
    mp_prec_t Ny = MPFR_PREC(y);   /* Precision of output variable */
    mp_prec_t Nt;                  /* Precision of the intermediary variable */
    mp_exp_t  err, exp_te, exp_ti; /* Precision of error */
    MPFR_ZIV_DECL (loop);

    /* compute the precision of intermediary variable */
    /* the optimal number of bits : see algorithms.tex */
    Nt = Ny + 4 + MPFR_INT_CEIL_LOG2 (Ny);

    /* initialization of intermediary variables */
    mpfr_init2 (t, Nt);

    /* First computation of acosh */
    MPFR_ZIV_INIT (loop, Nt);
    for (;;)
      {
        /* compute acosh */
        mpfr_mul (t, x, x, GMP_RNDD);      /* x^2 */
        exp_te = MPFR_GET_EXP (t);
        mpfr_sub_ui (t, t, 1, GMP_RNDD);   /* x^2-1 */
        exp_ti = MPFR_GET_EXP (t);
        mpfr_sqrt (t, t, GMP_RNDN);        /* sqrt(x^2-1) */
        mpfr_add (t, t, x, GMP_RNDN);      /* sqrt(x^2-1)+x */
        mpfr_log (t, t, GMP_RNDN);         /* ln(sqrt(x^2-1)+x)*/

        /* error estimate -- see algorithms.tex */
        err = 3 + MAX (1, exp_te - exp_ti) - MPFR_GET_EXP(t);
        /* error is bounded by 1/2 + 2^err <= 2^(1+max(-1,err)) */
        err = 1 + MAX (-1, err);
        if (MPFR_LIKELY (MPFR_CAN_ROUND (t, Nt - err, Ny, rnd_mode)))
          break;

        /* reactualisation of the precision */
        MPFR_ZIV_NEXT (loop, Nt);
        mpfr_set_prec (t, Nt);
      }
    MPFR_ZIV_FREE (loop);

    inexact = mpfr_set (y, t, rnd_mode);

    mpfr_clear (t);
  }

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (y, inexact, rnd_mode);
}






