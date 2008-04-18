/* mpfr_hypot -- Euclidean distance

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

/* The computation of hypot of x and y is done by  *
 *    hypot(x,y)= sqrt(x^2+y^2) = z                */

int
mpfr_hypot (mpfr_ptr z, mpfr_srcptr x, mpfr_srcptr y, mp_rnd_t rnd_mode)
{
  int inexact, exact;
  mpfr_t t, te, ti; /* auxiliary variables */
  mp_prec_t Nx, Ny, Nz; /* size variables */
  mp_prec_t Nt;   /* precision of the intermediary variable */
  mp_exp_t Ex, Ey, sh;
  mp_exp_unsigned_t diff_exp;
  MPFR_SAVE_EXPO_DECL (expo);
  MPFR_ZIV_DECL (loop);

  /* particular cases */
  if (MPFR_ARE_SINGULAR (x, y))
    {
      if (MPFR_IS_INF (x) || MPFR_IS_INF (y))
        {
          /* Return +inf, even when the other number is NaN. */
          MPFR_SET_INF (z);
          MPFR_SET_POS (z);
          MPFR_RET (0);
        }
      else if (MPFR_IS_NAN (x) || MPFR_IS_NAN (y))
        {
          MPFR_SET_NAN (z);
          MPFR_RET_NAN;
        }
      else if (MPFR_IS_ZERO (x))
        return mpfr_abs (z, y, rnd_mode);
      else /* y is necessarily 0 */
        return mpfr_abs (z, x, rnd_mode);
    }
  MPFR_CLEAR_FLAGS(z);

  if (mpfr_cmpabs (x, y) < 0)
    {
      mpfr_srcptr t;
      t = x;
      x = y;
      y = t;
    }

  /* now |x| >= |y| */

  Ex = MPFR_GET_EXP (x);
  Ey = MPFR_GET_EXP (y);
  diff_exp = (mp_exp_unsigned_t) Ex - Ey;

  Nx = MPFR_PREC (x);   /* Precision of input variable */
  Nz = MPFR_PREC (z);   /* Precision of output variable */

  /* we have x < 2^Ex thus x^2 < 2^(2*Ex),
     and ulp(x) = 2^(Ex-Nx) thus ulp(x^2) >= 2^(2*Ex-2*Nx).
     y does not overlap with the result when
     x^2+y^2 < (|x| + 1/2*ulp(x,Nz))^2 = x^2 + |x|*ulp(x,Nz) + 1/4*ulp(x,Nz)^2,
     i.e. a sufficient condition is y^2 < |x|*ulp(x,Nz),
     or 2^(2*Ey) <= 2^(2*Ex-1-Nz), i.e. 2*diff_exp > Nz.
     Warning: this is true only for Nx <= Nz, otherwise the trailing bits
     of x may be already very close to 1/2*ulp(x,Nz)!
     If Nx > Nz, then we can notice that it is possible to round on Nx bits
     if 2*diff_exp > Nx (see above as if Nz = Nx), therefore on Nz bits.
     Hence the condition: 2*diff_exp > MAX(Nz,Nx).
  */
  if (diff_exp > MAX (Nz, Nx) / 2)
    /* result is |x| or |x|+ulp(|x|,Nz) */
    {
      if (MPFR_UNLIKELY (rnd_mode == GMP_RNDU))
        {
          /* If z > abs(x), then it was already rounded up; otherwise
             z = abs(x), and we need to add one ulp due to y. */
          if (mpfr_abs (z, x, rnd_mode) == 0)
            mpfr_nexttoinf (z);
          return 1;
        }
      else /* GMP_RNDZ, GMP_RNDD, GMP_RNDN */
        {
          if (MPFR_LIKELY (Nz >= Nx))
            {
              mpfr_abs (z, x, rnd_mode);  /* exact */
              return -1;
            }
          else
            {
              MPFR_SET_EXP (z, Ex);
              MPFR_SET_SIGN (z, 1);
              MPFR_RNDRAW_GEN (inexact, z, MPFR_MANT (x), Nx, rnd_mode, 1,
                               goto addoneulp,
                  if (MPFR_UNLIKELY (++MPFR_EXP (z) > __gmpfr_emax))
                    return mpfr_overflow (z, rnd_mode, 1);
                              );
              return inexact ? inexact : -1;
            }
        }
    }

  /* General case */

  Ny = MPFR_PREC(y);   /* Precision of input variable */

  /* compute the working precision -- see algorithms.ps */
  Nt = MAX (MAX (MAX (Nx, Ny), Nz), 8);
  /* FIXME: if Nx or Ny are very large with respect to the target precision
     Nz, this may be overkill! */
  Nt = Nt + MPFR_INT_CEIL_LOG2 (Nt) + 2;

  /* initialise the intermediary variables */
  mpfr_init2 (t, Nt);
  mpfr_init2 (te, Nt);
  mpfr_init2 (ti, Nt);

  MPFR_SAVE_EXPO_MARK (expo);

  sh = MAX (0, MIN (Ex, Ey));

  MPFR_ZIV_INIT (loop, Nt);
  for (;;)
    {
      /* computations of hypot */
      mpfr_div_2ui (te, x, sh, GMP_RNDZ); /* exact since Nt >= Nx */
      mpfr_div_2ui (ti, y, sh, GMP_RNDZ); /* exact since Nt >= Ny */
      exact = mpfr_mul (te, te, te, GMP_RNDZ);    /* x^2 */
      exact |= mpfr_mul (ti, ti, ti, GMP_RNDZ);   /* y^2 */
      exact |= mpfr_add (t, te, ti, GMP_RNDZ);    /* x^2+y^2 */
      exact |= mpfr_sqrt (t, t, GMP_RNDZ);        /* sqrt(x^2+y^2)*/

      if (MPFR_LIKELY (exact == 0
                       || MPFR_CAN_ROUND (t, Nt-2, Nz, rnd_mode)))
        break;

      /* reactualization of the precision */
      MPFR_ZIV_NEXT (loop, Nt);
      mpfr_set_prec (t, Nt);
      mpfr_set_prec (te, Nt);
      mpfr_set_prec (ti, Nt);

    }
  MPFR_ZIV_FREE (loop);
  inexact = mpfr_mul_2ui (z, t, sh, rnd_mode);

  MPFR_ASSERTD (exact == 0 || inexact != 0);

  mpfr_clear (t);
  mpfr_clear (ti);
  mpfr_clear (te);

  /*
       exact  inexact
        0         0         result is exact, ternary flag is 0
        0       non zero    t is exact, ternary flag given by inexact
        1         0         impossible (see above)
        1       non zero    ternary flag given by inexact
   */

  MPFR_SAVE_EXPO_FREE (expo);

  return mpfr_check_range (z, inexact, rnd_mode);
}
