/* mpc_exp -- exponential of a complex number.

Copyright (C) 2002, 2009 Andreas Enge, Paul Zimmermann, Philippe Th\'eveny

This file is part of the MPC Library.

The MPC Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPC Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPC Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include "mpc-impl.h"

int
mpc_exp (mpc_ptr rop, mpc_srcptr op, mpc_rnd_t rnd)
{
  mpfr_t x, y, z;
  mp_prec_t prec;
  int ok = 0;
  int inex_re, inex_im;

  /* let op = a + i*b, then exp(op) = exp(a)*[cos(b) + i*sin(b)]
                                    = exp(a)*cos(b) + i*exp(a)*sin(b).

     We use the following algorithm (same for the imaginary part):

     (1) x = o(exp(a)) rounded towards +infinity:
     (2) y = o(cos(b)) rounded to nearest
     (3) r = o(x*y)
     then the error on r for the real part is at most 4 ulps:
     |r - exp(a)*cos(b)| <= ulp(r) + |x*y - exp(a)*cos(b)|
                         <= ulp(r) + |x*y - exp(a)*y| + exp(a) * |y - cos(b)|
                         <= ulp(r) + |y| ulp(x) + 1/2 * x * ulp(y)
                         <= ulp(r) + 2 * ulp(x*y) + ulp(x*y) [Rule 4]
                         <= 4 * ulp(r) [Rule 8]
  */
  
  /* special values */
  if (mpfr_nan_p (MPC_RE (op)) || mpfr_nan_p (MPC_IM (op)))
    /* NaNs 
       exp(nan +i*y) = nan -i*0   if y = -0,
                       nan +i*0   if y = +0,
                       nan +i*nan otherwise
       exp(x+i*nan) =   +/-0 +/-i*0 if x=-inf,
                      +/-inf +i*nan if x=+inf,
                         nan +i*nan otherwise */
    {
      if (mpfr_zero_p (MPC_IM (op)))
        return mpc_set (rop, op, MPC_RNDNN);

      if (mpfr_inf_p (MPC_RE (op)))
        {
          if (mpfr_signbit (MPC_RE (op)))
            return mpc_set_ui_ui (rop, 0, 0, MPC_RNDNN);
          else
            {
              mpfr_set_inf (MPC_RE (rop), +1);
              mpfr_set_nan (MPC_IM (rop));
              return MPC_INEX(0, 0); /* Inf/NaN are exact */
            }
        }
      mpfr_set_nan (MPC_RE (rop));
      mpfr_set_nan (MPC_IM (rop));
      return MPC_INEX(0, 0); /* NaN is exact */
    }


  if (mpfr_zero_p (MPC_IM(op)))
    /* special case when the input is real 
       exp(x-i*0) = exp(x) -i*0, even if x is NaN
       exp(x+i*0) = exp(x) +i*0, even if x is NaN */
    {
      inex_re = mpfr_exp (MPC_RE(rop), MPC_RE(op), MPC_RND_RE(rnd));
      inex_im = mpfr_set (MPC_IM(rop), MPC_IM(op), MPC_RND_IM(rnd));
      return MPC_INEX(inex_re, inex_im);
    }

  if (mpfr_zero_p (MPC_RE (op)))
    /* special case when the input is imaginary  */
    {
      inex_re = mpfr_cos (MPC_RE (rop), MPC_IM (op), MPC_RND_RE(rnd));
      inex_im = mpfr_sin (MPC_IM (rop), MPC_IM (op), MPC_RND_IM(rnd));
      return MPC_INEX(inex_re, inex_im);
    }


  if (mpfr_inf_p (MPC_RE (op)))
    /* real part is an infinity, 
       exp(-inf +i*y) = 0*(cos y +i*sin y)
       exp(+inf +i*y) = +/-inf +i*nan         if y = +/-inf
                        +inf*(cos y +i*sin y) if 0 < |y| < inf */
    {
      mpfr_t n;

      mpfr_init2 (n, 2);
      if (mpfr_signbit (MPC_RE (op)))
        mpfr_set_ui (n, 0, GMP_RNDN);
      else
        mpfr_set_inf (n, +1);
      
      if (mpfr_inf_p (MPC_IM (op)))
        {
          inex_re = mpfr_set (MPC_RE (rop), n, GMP_RNDN);
          if (mpfr_signbit (MPC_RE (op)))
            inex_im = mpfr_set (MPC_IM (rop), n, GMP_RNDN);
          else
            {
              mpfr_set_nan (MPC_IM (rop));
              inex_im = 0; /* NaN is exact */
            }
        }
      else
        {
          mpfr_t c, s;
          mpfr_init2 (c, 2);
          mpfr_init2 (s, 2);

          mpfr_sin_cos (s, c, MPC_IM (op), GMP_RNDN);
          inex_re = mpfr_copysign (MPC_RE (rop), n, c, GMP_RNDN);
          inex_im = mpfr_copysign (MPC_IM (rop), n, s, GMP_RNDN);

          mpfr_clear (s);
          mpfr_clear (c);
        }

      mpfr_clear (n);
      return MPC_INEX(inex_re, inex_im);
    }

  if (mpfr_inf_p (MPC_IM (op)))
    /* real part is finite non-zero number, imaginary part is an infinity */
    {
      mpfr_set_nan (MPC_RE (rop));
      mpfr_set_nan (MPC_IM (rop));
      return MPC_INEX(0, 0); /* NaN is exact */
    }


  /* from now on, both parts of op are regular numbers */

  prec = MPC_MAX_PREC(rop);

  mpfr_init2 (x, 2);
  mpfr_init2 (y, 2);
  mpfr_init2 (z, 2);

  do
    {
      prec += mpc_ceil_log2 (prec) + 5;

      mpfr_set_prec (x, prec);
      mpfr_set_prec (y, prec);
      mpfr_set_prec (z, prec);

      /* FIXME: x may overflow so x.y does overflow too, while Re(exp(op))
         could be represented in the precision of rop. */
      mpfr_exp (x, MPC_RE(op), GMP_RNDN);
      mpfr_sin_cos (z, y, MPC_IM(op), GMP_RNDN);
      mpfr_mul (y, y, x, GMP_RNDN);

      ok = mpfr_inf_p (y) || mpfr_zero_p (x)
        || mpfr_can_round (y, prec - 2, GMP_RNDN, GMP_RNDZ,
                       MPFR_PREC(MPC_RE(rop)) + (MPC_RND_RE(rnd) == GMP_RNDN));
      if (ok) /* compute imaginary part */
        {
          mpfr_mul (z, z, x, GMP_RNDN);
          ok = mpfr_inf_p (z) || mpfr_zero_p (x)
            || mpfr_can_round (z, prec - 2, GMP_RNDN, GMP_RNDZ,
                       MPFR_PREC(MPC_IM(rop)) + (MPC_RND_IM(rnd) == GMP_RNDN));
        }
    }
  while (ok == 0);

  inex_re = mpfr_set (MPC_RE(rop), y, MPC_RND_RE(rnd));
  inex_im = mpfr_set (MPC_IM(rop), z, MPC_RND_IM(rnd));

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
  
  return MPC_INEX(inex_re, inex_im);
}
