/* mpfr_pow -- power function x^y

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

/* return non zero iff x^y is exact.
   Assumes x and y are ordinary numbers,
   y is not an integer, x is not a power of 2 and x is positive

   If x^y is exact, it computes it and sets *inexact.
*/
static int
mpfr_pow_is_exact (mpfr_ptr z, mpfr_srcptr x, mpfr_srcptr y,
                   mp_rnd_t rnd_mode, int *inexact)
{
  mpz_t a, c;
  mp_exp_t d, b;
  unsigned long i;
  int res;

  MPFR_ASSERTD (!MPFR_IS_SINGULAR (y));
  MPFR_ASSERTD (!MPFR_IS_SINGULAR (x));
  MPFR_ASSERTD (!mpfr_integer_p (y));
  MPFR_ASSERTD (mpfr_cmp_si_2exp (x, MPFR_INT_SIGN (x),
                                  MPFR_GET_EXP (x) - 1) != 0);
  MPFR_ASSERTD (MPFR_IS_POS (x));

  if (MPFR_IS_NEG (y))
    return 0; /* x is not a power of two => x^-y is not exact */

  /* compute d such that y = c*2^d with c odd integer */
  mpz_init (c);
  d = mpfr_get_z_exp (c, y);
  i = mpz_scan1 (c, 0);
  mpz_div_2exp (c, c, i);
  d += i;
  /* now y=c*2^d with c odd */
  /* Since y is not an integer, d is necessarily < 0 */
  MPFR_ASSERTD (d < 0);

  /* Compute a,b such that x=a*2^b */
  mpz_init (a);
  b = mpfr_get_z_exp (a, x);
  i = mpz_scan1 (a, 0);
  mpz_div_2exp (a, a, i);
  b += i;
  /* now x=a*2^b with a is odd */

  for (res = 1 ; d != 0 ; d++)
    {
      /* a*2^b is a square iff
            (i)  a is a square when b is even
            (ii) 2*a is a square when b is odd */
      if (b % 2 != 0)
        {
          mpz_mul_2exp (a, a, 1); /* 2*a */
          b --;
        }
      MPFR_ASSERTD ((b % 2) == 0);
      if (!mpz_perfect_square_p (a))
        {
          res = 0;
          goto end;
        }
      mpz_sqrt (a, a);
      b = b / 2;
    }
  /* Now x = (a'*2^b')^(2^-d) with d < 0
     so x^y = ((a'*2^b')^(2^-d))^(c*2^d)
            = ((a'*2^b')^c with c odd integer */
  {
    mpfr_t tmp;
    mp_prec_t p;
    MPFR_MPZ_SIZEINBASE2 (p, a);
    mpfr_init2 (tmp, p); /* prec = 1 should not be possible */
    res = mpfr_set_z (tmp, a, GMP_RNDN);
    MPFR_ASSERTD (res == 0);
    res = mpfr_mul_2si (tmp, tmp, b, GMP_RNDN);
    MPFR_ASSERTD (res == 0);
    *inexact = mpfr_pow_z (z, tmp, c, rnd_mode);
    mpfr_clear (tmp);
    res = 1;
  }
 end:
  mpz_clear (a);
  mpz_clear (c);
  return res;
}

/* Return 1 if y is an odd integer, 0 otherwise. */
static int
is_odd (mpfr_srcptr y)
{
  mp_exp_t expo;
  mp_prec_t prec;
  mp_size_t yn;
  mp_limb_t *yp;

  /* NAN, INF or ZERO are not allowed */
  MPFR_ASSERTD (!MPFR_IS_SINGULAR (y));

  expo = MPFR_GET_EXP (y);
  if (expo <= 0)
    return 0;  /* |y| < 1 and not 0 */

  prec = MPFR_PREC(y);
  if ((mpfr_prec_t) expo > prec)
    return 0;  /* y is a multiple of 2^(expo-prec), thus not odd */

  /* 0 < expo <= prec:
     y = 1xxxxxxxxxt.zzzzzzzzzzzzzzzzzz[000]
          expo bits   (prec-expo) bits

     We have to check that:
     (a) the bit 't' is set
     (b) all the 'z' bits are zero
  */

  prec = ((prec - 1) / BITS_PER_MP_LIMB + 1) * BITS_PER_MP_LIMB - expo;
  /* number of z+0 bits */

  yn = prec / BITS_PER_MP_LIMB;
  MPFR_ASSERTN(yn >= 0);
  /* yn is the index of limb containing the 't' bit */

  yp = MPFR_MANT(y);
  /* if expo is a multiple of BITS_PER_MP_LIMB, t is bit 0 */
  if (expo % BITS_PER_MP_LIMB == 0 ? (yp[yn] & 1) == 0
      : yp[yn] << ((expo % BITS_PER_MP_LIMB) - 1) != MPFR_LIMB_HIGHBIT)
    return 0;
  while (--yn >= 0)
    if (yp[yn] != 0)
      return 0;
  return 1;
}

/* The computation of z = pow(x,y) is done by
   z = exp(y * log(x)) = x^y
   For the special cases, see Section F.9.4.4 of the C standard:
     _ pow(±0, y) = ±inf for y an odd integer < 0.
     _ pow(±0, y) = +inf for y < 0 and not an odd integer.
     _ pow(±0, y) = ±0 for y an odd integer > 0.
     _ pow(±0, y) = +0 for y > 0 and not an odd integer.
     _ pow(-1, ±inf) = 1.
     _ pow(+1, y) = 1 for any y, even a NaN.
     _ pow(x, ±0) = 1 for any x, even a NaN.
     _ pow(x, y) = NaN for finite x < 0 and finite non-integer y.
     _ pow(x, -inf) = +inf for |x| < 1.
     _ pow(x, -inf) = +0 for |x| > 1.
     _ pow(x, +inf) = +0 for |x| < 1.
     _ pow(x, +inf) = +inf for |x| > 1.
     _ pow(-inf, y) = -0 for y an odd integer < 0.
     _ pow(-inf, y) = +0 for y < 0 and not an odd integer.
     _ pow(-inf, y) = -inf for y an odd integer > 0.
     _ pow(-inf, y) = +inf for y > 0 and not an odd integer.
     _ pow(+inf, y) = +0 for y < 0.
     _ pow(+inf, y) = +inf for y > 0. */
int
mpfr_pow (mpfr_ptr z, mpfr_srcptr x, mpfr_srcptr y, mp_rnd_t rnd_mode)
{
  int inexact;
  int cmp_x_1;
  MPFR_SAVE_EXPO_DECL (expo);

  MPFR_LOG_FUNC (("x[%#R]=%R y[%#R]=%R rnd=%d", x, x, y, y, rnd_mode),
                 ("z[%#R]=%R inexact=%d", z, z, inexact));

  if (MPFR_ARE_SINGULAR (x, y))
    {
      /* pow(x, 0) returns 1 for any x, even a NaN. */
      if (MPFR_UNLIKELY (MPFR_IS_ZERO (y)))
        return mpfr_set (z, __gmpfr_one, rnd_mode);
      else if (MPFR_IS_NAN (x))
        {
          MPFR_SET_NAN (z);
          MPFR_RET_NAN;
        }
      else if (MPFR_IS_NAN (y))
        {
          /* pow(+1, NaN) returns 1. */
          if (mpfr_cmp (x, __gmpfr_one) == 0)
            return mpfr_set (z, __gmpfr_one, rnd_mode);
          MPFR_SET_NAN (z);
          MPFR_RET_NAN;
        }
      else if (MPFR_IS_INF (y))
        {
          if (MPFR_IS_INF (x))
            {
              if (MPFR_IS_POS (y))
                MPFR_SET_INF (z);
              else
                MPFR_SET_ZERO (z);
              MPFR_SET_POS (z);
              MPFR_RET (0);
            }
          else
            {
              int cmp;
              cmp = mpfr_cmpabs (x, __gmpfr_one) * MPFR_INT_SIGN (y);
              MPFR_SET_POS (z);
              if (cmp > 0)
                {
                  /* Return +inf. */
                  MPFR_SET_INF (z);
                  MPFR_RET (0);
                }
              else if (cmp < 0)
                {
                  /* Return +0. */
                  MPFR_SET_ZERO (z);
                  MPFR_RET (0);
                }
              else
                {
                  /* Return 1. */
                  return mpfr_set (z, __gmpfr_one, rnd_mode);
                }
            }
        }
      else if (MPFR_IS_INF (x))
        {
          int negative;
          /* Determine the sign now, in case y and z are the same object */
          negative = MPFR_IS_NEG (x) && is_odd (y);
          if (MPFR_IS_POS (y))
            MPFR_SET_INF (z);
          else
            MPFR_SET_ZERO (z);
          if (negative)
            MPFR_SET_NEG (z);
          else
            MPFR_SET_POS (z);
          MPFR_RET (0);
        }
      else
        {
          int negative;
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          /* Determine the sign now, in case y and z are the same object */
          negative = MPFR_IS_NEG(x) && is_odd (y);
          if (MPFR_IS_NEG (y))
            MPFR_SET_INF (z);
          else
            MPFR_SET_ZERO (z);
          if (negative)
            MPFR_SET_NEG (z);
          else
            MPFR_SET_POS (z);
          MPFR_RET (0);
        }
    }

  cmp_x_1 = mpfr_cmp (x, __gmpfr_one);
  if (cmp_x_1 == 0) /* 1^y is always 1 */
    return mpfr_set (z, __gmpfr_one, rnd_mode);

  /* detect overflows: |x^y| >= 2^EMAX when (EXP(x)-1) * y >= EMAX for y > 0,
                                       or   EXP(x) * y     >= EMAX for y < 0 */
  {
    double exy;
    int negative;

    exy = (double) (mpfr_sgn (y) > 0) ? MPFR_EXP(x) - 1 : MPFR_EXP(x);
    exy *= mpfr_get_d (y, GMP_RNDZ);
    if (exy >= (double) __gmpfr_emax)
      {
        negative = MPFR_SIGN(x) < 0 && is_odd (y);
        return mpfr_overflow (z, rnd_mode, negative ? -1 : 1);
      }
  }

  /* detect underflows: for x > 0, y < 0, |x^y| = |(1/x)^(-y)|
                        <= 2^((1-EXP(x))*(-y)) */
  if (MPFR_IS_NEG(y) && MPFR_EXP(x) > 1)
  {
    mpfr_t tmp;
    int underflow;

    /* We must restore the flags if no underflow. */
    MPFR_SAVE_EXPO_MARK (expo);
    mpfr_init2 (tmp, 53);
    mpfr_neg (tmp, y, GMP_RNDZ);
    mpfr_mul_si (tmp, tmp, 1 - MPFR_EXP(x), GMP_RNDZ);
    underflow = mpfr_cmp_si (tmp, __gmpfr_emin - 2) <= 0;
    mpfr_clear (tmp);
    MPFR_SAVE_EXPO_FREE (expo);
    if (underflow)
      /* warning: mpfr_underflow rounds away from 0 for GMP_RNDN */
      return mpfr_underflow (z, (rnd_mode == GMP_RNDN) ? GMP_RNDZ : rnd_mode, 1);
  }

  /* If y is an integer, we can use mpfr_pow_z (based on multiplications),
     but if y is very large (I'm not sure about the best threshold -- VL),
     we shouldn't use it, as it can be very slow and take a lot of memory
     (and even crash or make other programs crash, as several hundred of
     MBs may be necessary). */
  if (mpfr_integer_p (y) && MPFR_GET_EXP (y) <= 256)
    {
      mpz_t zi;

      mpz_init (zi);
      mpfr_get_z (zi, y, GMP_RNDN);
      inexact = mpfr_pow_z (z, x, zi, rnd_mode);
      mpz_clear (zi);
      return inexact;
    }

  /* x^y for x<0 and y not an integer is not defined */
  if (MPFR_IS_NEG (x))
    {
      MPFR_SET_NAN (z);
      MPFR_RET_NAN;
    }

  /* Special case (2^b)^Y which could be exact */
  if (mpfr_cmp_si_2exp (x, 1, MPFR_GET_EXP (x) - 1) == 0)
    {
      mpfr_t tmp;
      mp_exp_t b;
      /* now x = 2^b, so x^y = 2^(b*y) is exact whenever b*y is an integer */
      b = MPFR_GET_EXP (x) - 1; /* x = 2^b */
      mpfr_init2 (tmp, MPFR_PREC (y) + BITS_PER_MP_LIMB);
      inexact = mpfr_mul_si (tmp, y, b, GMP_RNDN); /* exact */
      MPFR_ASSERTD (inexact == 0);
      inexact = mpfr_exp2 (z, tmp, rnd_mode);
      mpfr_clear (tmp);
      return inexact;
    }

  MPFR_SAVE_EXPO_MARK (expo);

  /* Case where |y * log(x)| is very small. */
  {
    mpfr_t t;
    mp_exp_t err;

    /* We need an upper bound on the exponent of y * log(x). */
    mpfr_init2 (t, 16);
    mpfr_log (t, x, cmp_x_1 < 0 ? GMP_RNDD : GMP_RNDU); /* round away from 0 */
    MPFR_ASSERTN (MPFR_IS_PURE_FP (t));
    err = MPFR_GET_EXP (y) + MPFR_GET_EXP (t);
    mpfr_clear (t);
    mpfr_clear_flags ();
    MPFR_SMALL_INPUT_AFTER_SAVE_EXPO (z, __gmpfr_one, - err, 0,
                                      (MPFR_SIGN (y) > 0) ^ (cmp_x_1 < 0),
                                      rnd_mode, expo, {});
  }

  /* General case */
  {
    /* Declaration of the intermediary variable */
    mpfr_t t, u, k;
    int k_non_zero = 0;
    int check_exact_case = 0;
    /* Declaration of the size variable */
    mp_prec_t Nz = MPFR_PREC(z);               /* target precision */
    mp_prec_t Nt;                              /* working precision */
    mp_exp_t err, exp_te;                      /* error */
    MPFR_ZIV_DECL (ziv_loop);

    /* compute the precision of intermediary variable */
    /* the optimal number of bits : see algorithms.tex */
    Nt = Nz + 5 + MPFR_INT_CEIL_LOG2 (Nz);

    /* initialise of intermediary variable */
    mpfr_init2 (t, Nt);

    MPFR_ZIV_INIT (ziv_loop, Nt);
    for (;;)
      {
        /* compute exp(y*ln(x)) */
        /* TODO: explain why GMP_RNDU is used. */
        mpfr_log (t, x, GMP_RNDU);               /* ln(x) */
        mpfr_mul (t, y, t, GMP_RNDU);            /* y*ln(x) */
        if (k_non_zero)
          {
            mpfr_const_log2 (u, GMP_RNDD);
            mpfr_mul (u, u, k, GMP_RNDD);
            /* Error on u = k * log(2): < k * 2^(-Nt) < 1. */
            mpfr_sub (t, t, u, GMP_RNDU);
          }
        exp_te = MPFR_GET_EXP (t);               /* FIXME: May overflow */
        mpfr_exp (t, t, GMP_RNDN);               /* exp(y*ln(x))*/
        if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (t)))
          {
            mp_prec_t Ntmin;

            MPFR_ASSERTN (!k_non_zero);
            MPFR_ASSERTN (!MPFR_IS_NAN (t));
            if (MPFR_IS_ZERO (t))
              {
                /* Underflow. We computed rndn(exp(t)), where t >= y*ln(x).
                   Therefore rndn(x^y) = 0, and we have a real underflow on
                   x^y. */
                inexact = mpfr_underflow (z, rnd_mode == GMP_RNDN ? GMP_RNDZ
                                          : rnd_mode, MPFR_SIGN_POS);
                MPFR_SAVE_EXPO_UPDATE_FLAGS (expo, MPFR_FLAGS_INEXACT
                                             | MPFR_FLAGS_UNDERFLOW);
                break;
              }

            /* Overflow. */
            /* Note: we can probably use a low precision for this test. */
            mpfr_clear_flags ();
            mpfr_log (t, x, GMP_RNDD);               /* ln(x) */
            mpfr_mul (t, y, t, GMP_RNDD);            /* y*ln(x) */
            mpfr_exp (t, t, GMP_RNDD);               /* exp(y*ln(x))*/
            if (mpfr_overflow_p ())
              {
                /* We have computed a lower bound on x^y, and it overflowed.
                   Therefore we have a real overflow on x^y. */
                inexact = mpfr_overflow (z, rnd_mode, MPFR_SIGN_POS);
                MPFR_SAVE_EXPO_UPDATE_FLAGS (expo, MPFR_FLAGS_INEXACT
                                             | MPFR_FLAGS_OVERFLOW);
                break;
              }

            k_non_zero = 1;
            Ntmin = sizeof(mp_exp_t) * CHAR_BIT;
            if (Ntmin > Nt)
              {
                Nt = Ntmin;
                mpfr_set_prec (t, Nt);
              }
            mpfr_init2 (u, Nt);
            mpfr_init2 (k, Ntmin);
            mpfr_log2 (k, x, GMP_RNDN);
            mpfr_mul (k, y, k, GMP_RNDN);
            mpfr_round (k, k);
            /* |y| < 2^Ntmin, therefore |k| < 2^Nt. */
            continue;
          }
        /* estimate of the error -- see pow function in algorithms.tex.
           The error on t is at most 1/2 + 3*2^(exp_te+1) ulps, which is
           <= 2^(exp_te+3) for exp_te >= -1, and <= 2 ulps for exp_te <= -2.
           Additional error if k_no_zero: treal = t * errk, with
           1 - |k| * 2^(-Nt) <= exp(-|k| * 2^(-Nt)) <= errk <= 1,
           i.e. additional absolute error <= 2^(EXP(k)+EXP(t)-Nt).
           Total error <= 2^err1 + 2^err2 <= 2^(max(err1,err2)+1). */
        err = exp_te >= -1 ? exp_te + 3 : 1;
        if (k_non_zero)
          {
            if (MPFR_GET_EXP (k) > err)
              err = MPFR_GET_EXP (k);
            err++;
          }
        if (MPFR_LIKELY (MPFR_CAN_ROUND (t, Nt - err, Nz, rnd_mode)))
          {
            inexact = mpfr_set (z, t, rnd_mode);
            break;
          }

        /* check exact power */
        if (check_exact_case == 0)
          {
            if (mpfr_pow_is_exact (z, x, y, rnd_mode, &inexact))
              break;
            check_exact_case = 1;
          }

        /* reactualisation of the precision */
        MPFR_ZIV_NEXT (ziv_loop, Nt);
        mpfr_set_prec (t, Nt);
        if (k_non_zero)
          mpfr_set_prec (u, Nt);
      }
    MPFR_ZIV_FREE (ziv_loop);

    if (k_non_zero)
      {
        int inex2;

        MPFR_ASSERTN (MPFR_EMAX_MAX <= LONG_MAX);
        mpfr_clear_flags ();
        inex2 = mpfr_mul_2si (z, z, mpfr_get_si (k, GMP_RNDN), rnd_mode);
        if (inex2)  /* underflow or overflow */
          {
            inexact = inex2;
            MPFR_SAVE_EXPO_UPDATE_FLAGS (expo, __gmpfr_flags);
          }
        mpfr_clears (u, k, (void *) 0);
      }
    mpfr_clear (t);
  }

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (z, inexact, rnd_mode);
}
