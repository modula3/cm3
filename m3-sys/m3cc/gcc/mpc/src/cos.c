/* mpc_cos -- cosine of a complex number.

Copyright (C) 2008, 2009 Philippe Th\'eveny, Andreas Enge

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
mpc_cos (mpc_ptr rop, mpc_srcptr op, mpc_rnd_t rnd)
{
  mpfr_t x, y, z;
  mp_prec_t prec;
  int ok = 0;
  int inex_re, inex_im;

  /* special values */
  if (!mpfr_number_p (MPC_RE (op)) || !mpfr_number_p (MPC_IM (op)))
    {
      if (mpfr_nan_p (MPC_RE (op)))
        {
          /* cos(NaN + i * NaN) = NaN + i * NaN */
          /* cos(NaN - i * Inf) = +Inf + i * NaN */
          /* cos(NaN + i * Inf) = +Inf + i * NaN */
          /* cos(NaN - i * 0) = NaN - i * 0 */
          /* cos(NaN + i * 0) = NaN + i * 0 */
          /* cos(NaN + i * y) = NaN + i * NaN, when y != 0 */
          if (mpfr_inf_p (MPC_IM (op)))
            mpfr_set_inf (MPC_RE (rop), +1);
          else
            mpfr_set_nan (MPC_RE (rop));

          if (mpfr_zero_p (MPC_IM (op)))
            mpfr_set (MPC_IM (rop), MPC_IM (op), MPC_RND_IM (rnd));
          else
            mpfr_set_nan (MPC_IM (rop));
        }
      else if (mpfr_nan_p (MPC_IM (op)))
        {
          /* cos(-Inf + i * NaN) = NaN + i * NaN */
          /* cos(+Inf + i * NaN) = NaN + i * NaN */
          /* cos(-0 + i * NaN) = NaN - i * 0 */
          /* cos(+0 + i * NaN) = NaN + i * 0 */
          /* cos(x + i * NaN) = NaN + i * NaN, when x != 0 */
          if (mpfr_zero_p (MPC_RE (op)))
            mpfr_set (MPC_IM (rop), MPC_RE (op), MPC_RND_IM (rnd));
          else
            mpfr_set_nan (MPC_IM (rop));

          mpfr_set_nan (MPC_RE (rop));
        }
      else if (mpfr_inf_p (MPC_RE (op)))
        {
          /* cos(-Inf -i*Inf) = cos(+Inf +i*Inf) = -Inf +i*NaN */
          /* cos(-Inf +i*Inf) = cos(+Inf -i*Inf) = +Inf +i*NaN */
          /* cos(-Inf -i*0) = cos(+Inf +i*0) = NaN -i*0 */
          /* cos(-Inf +i*0) = cos(+Inf -i*0) = NaN +i*0 */
          /* cos(-Inf +i*y) = cos(+Inf +i*y) = NaN +i*NaN, when y != 0 */

          /* SAME_SIGN is useful only if op == (+/-)Inf + i * (+/-)0, but, as
             MPC_RE(OP) might be erased (when ROP == OP), we compute it now */
          const int same_sign =
            mpfr_signbit (MPC_RE (op)) == mpfr_signbit (MPC_IM (op));

          if (mpfr_inf_p (MPC_IM (op)))
            mpfr_set_inf (MPC_RE (rop), (same_sign ? -1 : +1));
          else
            mpfr_set_nan (MPC_RE (rop));

          if (mpfr_zero_p (MPC_IM (op)))
            mpfr_setsign (MPC_IM (rop), MPC_IM (op), same_sign,
                          MPC_RND_IM(rnd));
          else
            mpfr_set_nan (MPC_IM (rop));
        }
      else if (mpfr_zero_p (MPC_RE (op)))
        {
          /* cos(-0 -i*Inf) = cos(+0 +i*Inf) = +Inf -i*0 */
          /* cos(-0 +i*Inf) = cos(+0 -i*Inf) = +Inf +i*0 */
          const int same_sign =
            mpfr_signbit (MPC_RE (op)) == mpfr_signbit (MPC_IM (op));

          mpfr_setsign (MPC_IM (rop), MPC_RE (op), same_sign,
                        MPC_RND_IM (rnd));
          mpfr_set_inf (MPC_RE (rop), +1);
        }
      else
        {
          /* cos(x -i*Inf) = +Inf*cos(x) +i*Inf*sin(x), when x != 0 */
          /* cos(x +i*Inf) = +Inf*cos(x) -i*Inf*sin(x), when x != 0 */
          mpfr_t c;
          mpfr_t s;

          mpfr_init (c);
          mpfr_init (s);

          mpfr_sin_cos (s, c, MPC_RE (op), GMP_RNDN);
          mpfr_set_inf (MPC_RE (rop), mpfr_sgn (c));
          mpfr_set_inf (MPC_IM (rop),
                        (mpfr_sgn (MPC_IM (op)) == mpfr_sgn (s) ? -1 : +1));

          mpfr_clear (s);
          mpfr_clear (c);
        }

      return MPC_INEX (0, 0); /* always exact */
    }

  if (mpfr_zero_p (MPC_RE (op)))
    {
      /* cos(-0 - i * y) = cos(+0 + i * y) = cosh(y) - i * 0
         cos(-0 + i * y) = cos(+0 - i * y) = cosh(y) + i * 0,
         when y >= 0 */

      /* When ROP == OP, the sign of 0 will be erased, so use it now */
      const int imag_sign =
        mpfr_signbit (MPC_RE (op)) ==  mpfr_signbit (MPC_IM (op));

      if (mpfr_zero_p (MPC_IM (op)))
        inex_re = mpfr_set_ui (MPC_RE (rop), 1, MPC_RND_RE (rnd));
      else
        inex_re = mpfr_cosh (MPC_RE (rop), MPC_IM (op), MPC_RND_RE (rnd));

      mpfr_set_ui (MPC_IM (rop), 0, MPC_RND_IM (rnd));
      mpfr_setsign (MPC_IM (rop), MPC_IM (rop), imag_sign, MPC_RND_IM (rnd));

      return MPC_INEX (inex_re, 0);
    }

  if (mpfr_zero_p (MPC_IM (op)))
    {
      /* cos(x +i*0) = cos(x) -i*0*sign(sin(x)) */
      /* cos(x -i*0) = cos(x) +i*0*sign(sin(x)) */
      mpfr_t sign;
      mpfr_init2 (sign, 2);
      mpfr_sin (sign, MPC_RE (op), GMP_RNDN);
      if (!mpfr_signbit (MPC_IM (op)))
         MPFR_CHANGE_SIGN (sign);

      inex_re = mpfr_cos (MPC_RE (rop), MPC_RE (op), MPC_RND_RE (rnd));

      mpfr_set_ui (MPC_IM (rop), 0ul, GMP_RNDN);
      if (mpfr_signbit (sign))
         MPFR_CHANGE_SIGN (MPC_IM (rop));

      mpfr_clear (sign);

      return MPC_INEX (inex_re, 0);
    }

  /* ordinary (non-zero) numbers */

  /* let op = a + i*b, then cos(op) = cos(a)*cosh(b) - i*sin(a)*sinh(b).

     We use the following algorithm (same for the imaginary part),
     with rounding to nearest for all operations, and working precision w:

     (1) x = o(cos(a))
     (2) y = o(cosh(b))
     (3) r = o(x*y)
     then the error on r is at most 4 ulps, since we can write
     r = cos(a)*cosh(b)*(1+t)^3 with |t| <= 2^(-w),
     thus for w >= 2, r = cos(a)*cosh(b)*(1+4*t) with |t| <= 2^(-w),
     thus the relative error is bounded by 4*2^(-w) <= 4*ulp(r).
  */

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

      mpfr_sin_cos (y, x, MPC_RE(op), GMP_RNDN);
      mpfr_cosh (z, MPC_IM(op), GMP_RNDN);
      mpfr_mul (x, x, z, GMP_RNDN);
      ok = mpfr_can_round (x, prec - 2, GMP_RNDN, GMP_RNDZ,
                      MPFR_PREC(MPC_RE(rop)) + (MPC_RND_RE(rnd) == GMP_RNDN));
      if (ok) /* compute imaginary part */
        {
          mpfr_sinh (z, MPC_IM(op), GMP_RNDN);
          mpfr_mul (y, y, z, GMP_RNDN);
          mpfr_neg (y, y, GMP_RNDN);
          ok = mpfr_can_round (y, prec - 2, GMP_RNDN, GMP_RNDZ,
                      MPFR_PREC(MPC_IM(rop)) + (MPC_RND_IM(rnd) == GMP_RNDN));
        }
    }
  while (ok == 0);

  inex_re = mpfr_set (MPC_RE(rop), x, MPC_RND_RE(rnd));
  inex_im = mpfr_set (MPC_IM(rop), y, MPC_RND_IM(rnd));

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);

  return MPC_INEX (inex_re, inex_im);
}
