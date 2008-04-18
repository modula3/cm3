/* mpfr_pow_ui-- compute the power of a floating-point
                                  by a machine integer

Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

/* sets x to y^n, and return 0 if exact, non-zero otherwise */
int
mpfr_pow_ui (mpfr_ptr x, mpfr_srcptr y, unsigned long int n, mp_rnd_t rnd)
{
  unsigned long m;
  mpfr_t res;
  mp_prec_t prec, err;
  int inexact;
  mp_rnd_t rnd1;
  MPFR_SAVE_EXPO_DECL (expo);
  MPFR_ZIV_DECL (loop);

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (y)))
    {
      if (MPFR_IS_NAN (y))
        {
          MPFR_SET_NAN (x);
          MPFR_RET_NAN;
        }
      else if (n == 0) /* y^0 = 1 for any y except NAN */
        {
          /* The return mpfr_set_ui is important as 1 isn't necessarily
             in the exponent range. */
          return mpfr_set_ui (x, 1, rnd);
        }
      else if (MPFR_IS_INF (y))
        {
          /* Inf^n = Inf, (-Inf)^n = Inf for n even, -Inf for n odd */
          if ((MPFR_IS_NEG (y)) && ((n & 1) == 1))
            MPFR_SET_NEG (x);
          else
            MPFR_SET_POS (x);
          MPFR_SET_INF (x);
          MPFR_RET (0);
        }
      else /* y is zero */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (y));
          /* 0^n = 0 for any n */
          MPFR_SET_ZERO (x);
          if (MPFR_IS_POS (y) || ((n & 1) == 0))
            MPFR_SET_POS (x);
          else
            MPFR_SET_NEG (x);
          MPFR_RET (0);
        }
    }
  else if (MPFR_UNLIKELY (n <= 2))
    {
      if (n < 1)
        /* y^0 = 1 for any y */
        return mpfr_set_ui (x, 1, rnd);
      else if (n == 1)
        /* y^1 = y */
        return mpfr_set (x, y, rnd);
      else
        /* y^2 = sqr(y) */
        return mpfr_mul (x, y, y, rnd);
    }

  /* Augment exponent range */
  MPFR_SAVE_EXPO_MARK (expo);
  __gmpfr_emin -= 3;  /* So that we can check for underflow properly */

  /* setup initial precision */
  prec = MPFR_PREC (x) + 3 + BITS_PER_MP_LIMB
    + MPFR_INT_CEIL_LOG2 (MPFR_PREC (x));
  mpfr_init2 (res, prec);

  rnd1 = MPFR_IS_POS (y) ? GMP_RNDU : GMP_RNDD; /* away */

  MPFR_ZIV_INIT (loop, prec);
  for (;;)
    {
      int i;
      for (m = n, i = 0; m; i++, m >>= 1)
        ;
      /* now 2^(i-1) <= n < 2^i */
      MPFR_ASSERTD (prec > (mpfr_prec_t) i);
      err = prec - 1 - (mpfr_prec_t) i;
      MPFR_ASSERTD (i >= 1);
      mpfr_clear_overflow ();
      mpfr_clear_underflow ();
      /* First step: compute square from y */
      inexact = mpfr_mul (res, y, y, GMP_RNDU);
      if (n & (1UL << (i-2)))
        inexact |= mpfr_mul (res, res, y, rnd1);
      for (i -= 3; i >= 0 && !mpfr_overflow_p () && !mpfr_underflow_p (); i--)
        {
          inexact |= mpfr_mul (res, res, res, GMP_RNDU);
          if (n & (1UL << i))
            inexact |= mpfr_mul (res, res, y, rnd1);
        }
      /* let r(n) be the number of roundings: we have r(2)=1, r(3)=2,
         and r(2n)=2r(n)+1, r(2n+1)=2r(n)+2, thus r(n)=n-1.
         Using Higham's method, to each rounding corresponds a factor
         (1-theta) with 0 <= theta <= 2^(1-p), thus at the end the
         absolute error is bounded by (n-1)*2^(1-p)*res <= 2*(n-1)*ulp(res)
         since 2^(-p)*x <= ulp(x). Since n < 2^i, this gives a maximal
         error of 2^(1+i)*ulp(res).
      */
      if (MPFR_LIKELY (inexact == 0
                       || mpfr_overflow_p () || mpfr_underflow_p ()
                       || MPFR_CAN_ROUND (res, err, MPFR_PREC (x), rnd)))
        break;
      /* Actualisation of the precision */
      MPFR_ZIV_NEXT (loop, prec);
      mpfr_set_prec (res, prec);
    }
  MPFR_ZIV_FREE (loop);

  inexact = mpfr_set (x, res, rnd);
  mpfr_clear (res);

  /* Check Overflow */
  if (MPFR_UNLIKELY (mpfr_overflow_p ()))
    {
      MPFR_SAVE_EXPO_FREE (expo);
      return mpfr_overflow (x, rnd,
                            (n % 2) ? MPFR_SIGN (y) : MPFR_SIGN_POS);
    }
  /* Check Underflow  */
  else if (MPFR_UNLIKELY (mpfr_underflow_p ()))
    {
      if (rnd == GMP_RNDN)
        rnd = GMP_RNDZ;
      MPFR_SAVE_EXPO_FREE (expo);
      return mpfr_underflow (x, rnd,
                             (n % 2) ? MPFR_SIGN(y) : MPFR_SIGN_POS);
    }

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (x, inexact, rnd);
}
