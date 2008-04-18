/* mpfr_pow_si -- power function x^y with y a signed int

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

/* The computation of y=pow(x,z) is done by
 *    y=pow_ui(x,z) if z>0
 * else
 *    y=1/pow_ui(x,z) if z<0
 */

int
mpfr_pow_si (mpfr_ptr y, mpfr_srcptr x, long int n, mp_rnd_t rnd)
{
  if (n >= 0)
    return mpfr_pow_ui (y, x, n, rnd);
  else
    {
      if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
        {
          if (MPFR_IS_NAN (x))
            {
              MPFR_SET_NAN (y);
              MPFR_RET_NAN;
            }
          else if (MPFR_IS_INF (x))
            {
              MPFR_SET_ZERO (y);
              if (MPFR_IS_POS (x) || ((unsigned) n & 1) == 0)
                MPFR_SET_POS (y);
              else
                MPFR_SET_NEG (y);
              MPFR_RET (0);
            }
          else /* x is zero */
            {
              MPFR_ASSERTD (MPFR_IS_ZERO (x));
              MPFR_SET_INF(y);
              if (MPFR_IS_POS (x) || ((unsigned) n & 1) == 0)
                MPFR_SET_POS (y);
              else
                MPFR_SET_NEG (y);
              MPFR_RET(0);
            }
        }
      MPFR_CLEAR_FLAGS (y);

      /* detect exact powers: x^(-n) is exact iff x is a power of 2 */
      if (mpfr_cmp_si_2exp (x, MPFR_SIGN(x), MPFR_EXP(x) - 1) == 0)
        {
          mp_exp_t expx = MPFR_EXP (x) - 1, expy;
          MPFR_ASSERTD (n < 0);
          /* Warning: n * expx may overflow!
           * Some systems (apparently alpha-freebsd) abort with
           * LONG_MIN / 1, and LONG_MIN / -1 is undefined.
           * Proof of the overflow checking. The expressions below are
           * assumed to be on the rational numbers, but the word "overflow"
           * still has its own meaning in the C context. / still denotes
           * the integer (truncated) division, and // denotes the exact
           * division.
           * - First, (__gmpfr_emin - 1) / n and (__gmpfr_emax - 1) / n
           *   cannot overflow due to the constraints on the exponents of
           *   MPFR numbers.
           * - If n = -1, then n * expx = - expx, which is representable
           *   because of the constraints on the exponents of MPFR numbers.
           * - If expx = 0, then n * expx = 0, which is representable.
           * - If n < -1 and expx > 0:
           *   + If expx > (__gmpfr_emin - 1) / n, then
           *           expx >= (__gmpfr_emin - 1) / n + 1
           *                > (__gmpfr_emin - 1) // n,
           *     and
           *           n * expx < __gmpfr_emin - 1,
           *     i.e.
           *           n * expx <= __gmpfr_emin - 2.
           *     This corresponds to an underflow, with a null result in
           *     the rounding-to-nearest mode.
           *   + If expx <= (__gmpfr_emin - 1) / n, then n * expx cannot
           *     overflow since 0 < expx <= (__gmpfr_emin - 1) / n and
           *           0 > n * expx >= n * ((__gmpfr_emin - 1) / n)
           *                        >= __gmpfr_emin - 1.
           * - If n < -1 and expx < 0:
           *   + If expx < (__gmpfr_emax - 1) / n, then
           *           expx <= (__gmpfr_emax - 1) / n - 1
           *                < (__gmpfr_emax - 1) // n,
           *     and
           *           n * expx > __gmpfr_emax - 1,
           *     i.e.
           *           n * expx >= __gmpfr_emax.
           *     This corresponds to an overflow (2^(n * expx) has an
           *     exponent > __gmpfr_emax).
           *   + If expx >= (__gmpfr_emax - 1) / n, then n * expx cannot
           *     overflow since 0 > expx >= (__gmpfr_emax - 1) / n and
           *           0 < n * expx <= n * ((__gmpfr_emax - 1) / n)
           *                        <= __gmpfr_emax - 1.
           * Note: one could use expx bounds based on MPFR_EXP_MIN and
           * MPFR_EXP_MAX instead of __gmpfr_emin and __gmpfr_emax. The
           * current bounds do not lead to noticeably slower code and
           * allow us to avoid a bug in Sun's compiler for Solaris/x86
           * (when optimizations are enabled).
           */
          expy =
            n != -1 && expx > 0 && expx > (__gmpfr_emin - 1) / n ?
            MPFR_EMIN_MIN - 2 /* Underflow */ :
            n != -1 && expx < 0 && expx < (__gmpfr_emax - 1) / n ?
            MPFR_EMAX_MAX /* Overflow */ : n * expx;
          return mpfr_set_si_2exp (y, n % 2 ? MPFR_INT_SIGN (x) : 1,
                                   expy, rnd);
        }

      /* General case */
      {
        /* Declaration of the intermediary variable */
        mpfr_t t;
        /* Declaration of the size variable */
        mp_prec_t Ny = MPFR_PREC (y);               /* target precision */
        mp_prec_t Nt;                              /* working precision */
        mp_exp_t  err;                             /* error */
        int inexact;
        unsigned long abs_n;
        MPFR_SAVE_EXPO_DECL (expo);
        MPFR_ZIV_DECL (loop);

        abs_n = - (unsigned long) n;

        /* compute the precision of intermediary variable */
        /* the optimal number of bits : see algorithms.tex */
        Nt = Ny + 3 + MPFR_INT_CEIL_LOG2 (Ny);

        MPFR_SAVE_EXPO_MARK (expo);

        /* initialise of intermediary   variable */
        mpfr_init2 (t, Nt);

        MPFR_ZIV_INIT (loop, Nt);
        for (;;)
          {
            /* compute 1/(x^n), with n > 0 */
            mpfr_pow_ui (t, x, abs_n, GMP_RNDN);
            mpfr_ui_div (t, 1, t, GMP_RNDN);
            /* FIXME: old code improved, but I think this is still incorrect. */
            if (MPFR_UNLIKELY (MPFR_IS_ZERO (t)))
              {
                MPFR_ZIV_FREE (loop);
                mpfr_clear (t);
                MPFR_SAVE_EXPO_FREE (expo);
                return mpfr_underflow (y, rnd == GMP_RNDN ? GMP_RNDZ : rnd,
                                       abs_n & 1 ? MPFR_SIGN (x) :
                                       MPFR_SIGN_POS);
              }
            if (MPFR_UNLIKELY (MPFR_IS_INF (t)))
              {
                MPFR_ZIV_FREE (loop);
                mpfr_clear (t);
                MPFR_SAVE_EXPO_FREE (expo);
                return mpfr_overflow (y, rnd, abs_n & 1 ? MPFR_SIGN (x) :
                                      MPFR_SIGN_POS);
              }
            /* error estimate -- see pow function in algorithms.ps */
            err = Nt - 3;
            if (MPFR_LIKELY (MPFR_CAN_ROUND (t, err, Ny, rnd)))
              break;

            /* actualisation of the precision */
            Nt += BITS_PER_MP_LIMB;
            mpfr_set_prec (t, Nt);
          }
        MPFR_ZIV_FREE (loop);

        inexact = mpfr_set (y, t, rnd);
        mpfr_clear (t);
        MPFR_SAVE_EXPO_FREE (expo);
        return mpfr_check_range (y, inexact, rnd);
      }
    }
}
