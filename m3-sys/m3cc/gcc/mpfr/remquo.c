/* mpfr_remquo and mpfr_remainder -- argument reduction functions

Copyright 2007, 2008 Free Software Foundation, Inc.
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

/* the following is a hack to avoid duplicating the code with a single file */
#ifndef _INSIDE_REMQUO

# define _INSIDE_REMQUO

# include "mpfr-impl.h"

  /* first define mpfr_remainder */
# include "remquo.c"

  /* now define mpfr_remquo */
  /* we return as many bits as we can, keeping just one bit for the sign */
# define WANTED_BITS (sizeof(long) * CHAR_BIT - 1)
# define REMQUO
# include "remquo.c"

#else

/*
  remquo/remainder works as follows:
  Let q = x/y rounded to the nearest integer (to the nearest even number
  in case x/y = n + 1/2 with n integer).
  Put x - q*y in rem, rounded according to rnd.
  The value stored in *quo has the sign of q, and agrees with q with
  the 2^n low order bits. In other words, *quo = q (mod 2^n) and *quo q >= 0.
  If rem is zero, then it has the sign of x.
  The returned 'int' is the inexact flag giving the place of rem wrt x - q*y.

  If x or y is NaN: *quo is undefined, rem is NaN.
  If x is Inf, whatever y: *quo is undefined, rem is NaN.
  If y is Inf, x not NaN nor Inf: *quo is 0, rem is x.
  If y is 0, whatever x: *quo is undefined, rem is NaN.
  If x is 0, whatever y (not NaN nor 0): *quo is 0, rem is x.

  Otherwise if x and y are neither NaN, Inf nor 0, q is always defined,
  thus *quo is.
  Since |x - q*y| <= y/2, no overflow is possible.
  Only an underflow is possible when y is very small.
 */

int
#ifdef REMQUO
mpfr_remquo    (mpfr_ptr rem, long *quo,
#else
mpfr_remainder (mpfr_ptr rem,
#endif
                mpfr_srcptr x, mpfr_srcptr y, mp_rnd_t rnd)
{
  mp_exp_t ex, ey;
  int compare, inex, q_is_odd, sign, signx = MPFR_SIGN(x);
  mpz_t mx, my, r;

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x) || MPFR_IS_SINGULAR (y)))
    {
      if (MPFR_IS_NAN (x) || MPFR_IS_NAN (y) || MPFR_IS_INF (x)
          || MPFR_IS_ZERO (y))
        {
          /* for remquo, quo is undefined */
          MPFR_SET_NAN (rem);
          MPFR_RET_NAN;
        }
      else /* either y is Inf and x is 0 or non-special,
              or x is 0 and y is non-special,
              in both cases the quotient is zero. */
        {
#ifdef REMQUO
          *quo = 0;
#endif
          return mpfr_set (rem, x, rnd);
        }
    }

  /* now neither x nor y is NaN, Inf or zero */

  mpz_init (mx);
  mpz_init (my);
  mpz_init (r);

  ex = mpfr_get_z_exp (mx, x); /* x = mx*2^ex */
  ey = mpfr_get_z_exp (my, y); /* y = my*2^ey */

  /* to get rid of sign problems, we compute it separately:
     quo(-x,-y) = quo(x,y), rem(-x,-y) = -rem(x,y)
     quo(-x,y) = -quo(x,y), rem(-x,y)  = -rem(x,y)
     thus quo = sign(x/y)*quo(|x|,|y|), rem = sign(x)*rem(|x|,|y|) */
  sign = (signx == MPFR_SIGN(y)) ? 1 : -1;
  mpz_abs (mx, mx);
  mpz_abs (my, my);

  /* divide my by 2^k if possible to make operations mod my easier */
  {
    unsigned long k = mpz_scan1 (my, 0);
    ey += k;
    mpz_div_2exp (my, my, k);
  }

  if (ex <= ey)
    {
      /* q = x/y = mx/(my*2^(ey-ex)) */
      mpz_mul_2exp (my, my, ey - ex); /* divide mx by my*2^(ey-ex) */
      mpz_fdiv_qr (mx, r, mx, my); /* 0 <= |r| <= |my|, r has the same
                                      sign as my */
      q_is_odd = mpz_tstbit (mx, 0);
#ifdef REMQUO /* mx is the quotient */
      mpz_tdiv_r_2exp (mx, mx, WANTED_BITS);
      *quo = mpz_get_si (mx);
#endif
    }
  else /* ex > ey */
    {
#ifdef REMQUO
      /* for remquo, to get the low WANTED_BITS more bits of the quotient,
         we first compute R =  X mod Y*2^WANTED_BITS, where X and Y are
         defined below. Then the low WANTED_BITS of the quotient are
         floor(R/Y). */
      mpz_mul_2exp (my, my, WANTED_BITS); /* 2^WANTED_BITS*Y */
#else
      /* Let X = mx*2^(ex-ey) and Y = my. Then both X and Y are integers.
         Assume X = R mod Y, then x = X*2^ey = R*2^ey mod (Y*2^ey=y).
         To be able to perform the rounding, we need the least significant
         bit of the quotient, i.e., one more bit in the remainder, which is
         obtained by dividing by 2Y.
      */
      mpz_mul_2exp (my, my, 1); /* 2Y */
#endif
      mpz_set_ui (r, 2);
      mpz_powm_ui (r, r, ex - ey, my); /* 2^(ex-ey) mod my */
      mpz_mul (r, r, mx);
      mpz_mod (r, r, my);
#ifdef REMQUO
      /* now 0 <= r < 2^WANTED_BITS*Y */
      mpz_div_2exp (my, my, WANTED_BITS); /* back to Y */
      mpz_tdiv_qr (mx, r, r, my);
      /* oldr = mx*my + newr */
      *quo = mpz_get_si (mx);
      q_is_odd = *quo & 1;
#else
      /* now 0 <= r < 2Y */
      mpz_div_2exp (my, my, 1); /* back to Y */
      q_is_odd = mpz_cmpabs (r, my) >= 0; /* least significant bit of q */
      if (q_is_odd)
        mpz_sub (r, r, my);
#endif
      /* now 0 <= |r| < |my|, and q_is_odd is the least significant bit of q */
    }

  if (mpz_cmp_ui (r, 0) == 0)
    inex = mpfr_set_ui (rem, 0, GMP_RNDN);
  else
    {
      /* FIXME: the comparison 2*r < my could be done more efficiently
         at the mpn level */
      mpz_mul_2exp (r, r, 1);
      compare = mpz_cmpabs (r, my);
      mpz_div_2exp (r, r, 1);
      compare = (compare > 0) || ((compare == 0) && q_is_odd);
      /* if compare != 0, we need to subtract my to r, and add 1 to quo */
      if (compare)
        {
          mpz_sub (r, r, my);
#ifdef REMQUO
          *quo += 1;
#endif
        }
      inex = mpfr_set_z (rem, r, rnd);
      /* if ex > ey, rem should be multiplied by 2^ey, else by 2^ex */
      MPFR_EXP(rem) += (ex > ey) ? ey : ex;
    }

#ifdef REMQUO
  *quo *= sign;
#endif

  /* take into account sign of x */
  if (signx < 0)
    {
      mpfr_neg (rem, rem, GMP_RNDN);
      inex = -inex;
    }

  mpz_clear (mx);
  mpz_clear (my);
  mpz_clear (r);

  return inex;
}

#endif /* _INSIDE_REMQUO */
