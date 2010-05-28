/* mpc_sqrt -- Take the square root of a complex number.

Copyright (C) 2002, 2008, 2009 Andreas Enge, Paul Zimmermann, Philippe Th\'eveny

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
mpc_sqrt (mpc_ptr a, mpc_srcptr b, mpc_rnd_t rnd)
{
  int ok_w, ok_t = 0;
  mpfr_t    w, t;
  mp_rnd_t  rnd_w, rnd_t;
  mp_prec_t prec_w, prec_t;
  /* the rounding mode and the precision required for w and t, which can */
  /* be either the real or the imaginary part of a */
  mp_prec_t prec;
  int inex_w, inex_t = 1, inex, loops = 0;
  /* comparison of the real/imaginary part of b with 0 */
  const int re_cmp = mpfr_cmp_ui (MPC_RE (b), 0);
  const int im_cmp = mpfr_cmp_ui (MPC_IM (b), 0);
  /* we need to know the sign of Im(b) when it is +/-0 */
  const int im_sgn = mpfr_signbit (MPC_IM (b)) == 0? 0 : -1;

  /* special values */
  /* sqrt(x +i*Inf) = +Inf +I*Inf, even if x = NaN */
  /* sqrt(x -i*Inf) = +Inf -I*Inf, even if x = NaN */
  if (mpfr_inf_p (MPC_IM (b)))
    {
      mpfr_set_inf (MPC_RE (a), +1);
      mpfr_set_inf (MPC_IM (a), im_sgn);
      return MPC_INEX (0, 0);
    }

  if (mpfr_inf_p (MPC_RE (b)))
    {
      if (mpfr_signbit (MPC_RE (b)))
        {
          if (mpfr_number_p (MPC_IM (b)))
            {
              /* sqrt(-Inf +i*y) = +0 +i*Inf, when y positive */
              /* sqrt(-Inf +i*y) = +0 -i*Inf, when y positive */
              mpfr_set_ui (MPC_RE (a), 0, GMP_RNDN);
              mpfr_set_inf (MPC_IM (a), im_sgn);
              return MPC_INEX (0, 0);
            }
          else
            {
              /* sqrt(-Inf +i*NaN) = NaN +/-i*Inf */
              mpfr_set_nan (MPC_RE (a));
              mpfr_set_inf (MPC_IM (a), im_sgn);
              return MPC_INEX (0, 0);
            }
        }
      else
        {
          if (mpfr_number_p (MPC_IM (b)))
            {
              /* sqrt(+Inf +i*y) = +Inf +i*0, when y positive */
              /* sqrt(+Inf +i*y) = +Inf -i*0, when y positive */
              mpfr_set_inf (MPC_RE (a), +1);
              mpfr_set_ui (MPC_IM (a), 0, GMP_RNDN);
              if (im_sgn)
                mpc_conj (a, a, MPC_RNDNN);
              return MPC_INEX (0, 0);
            }
          else
            {
              /* sqrt(+Inf -i*Inf) = +Inf -i*Inf */
              /* sqrt(+Inf +i*Inf) = +Inf +i*Inf */
              /* sqrt(+Inf +i*NaN) = +Inf +i*NaN */
              return mpc_set (a, b, rnd);
            }
        }
    }

  /* sqrt(x +i*NaN) = NaN +i*NaN, if x is not infinite */
  /* sqrt(NaN +i*y) = NaN +i*NaN, if y is not infinite */
  if (mpfr_nan_p (MPC_RE (b)) || mpfr_nan_p (MPC_IM (b)))
    {
      mpfr_set_nan (MPC_RE (a));
      mpfr_set_nan (MPC_IM (a));
      return MPC_INEX (0, 0);
    }

  /* purely real */
  if (im_cmp == 0)
    {
      if (re_cmp == 0)
        {
          mpc_set_ui_ui (a, 0, 0, MPC_RNDNN);
          if (im_sgn)
            mpc_conj (a, a, MPC_RNDNN);
          return MPC_INEX (0, 0);
        }
      else if (re_cmp > 0)
        {
          inex_w = mpfr_sqrt (MPC_RE (a), MPC_RE (b), MPC_RND_RE (rnd));
          mpfr_set_ui (MPC_IM (a), 0, GMP_RNDN);
          if (im_sgn)
            mpc_conj (a, a, MPC_RNDNN);
          return MPC_INEX (inex_w, 0);
        }
      else
        {
          mpfr_init2 (w, MPFR_PREC (MPC_RE (b)));
          mpfr_neg (w, MPC_RE (b), GMP_RNDN);
          if (im_sgn)
            {
              inex_w = -mpfr_sqrt (MPC_IM (a), w, INV_RND (MPC_RND_IM (rnd)));
              mpfr_neg (MPC_IM (a), MPC_IM (a), GMP_RNDN);
            }
          else
            inex_w = mpfr_sqrt (MPC_IM (a), w, MPC_RND_IM (rnd));

          mpfr_set_ui (MPC_RE (a), 0, GMP_RNDN);
          mpfr_clear (w);
          return MPC_INEX (0, inex_w);
        }
    }

  /* purely imaginary */
  if (re_cmp == 0)
    {
      mpfr_t y;

      y[0] = MPC_IM (b)[0];
      /* If y/2 underflows, so does sqrt(y/2) */
      mpfr_div_2ui (y, y, 1, GMP_RNDN);
      if (im_cmp > 0)
        {
          inex_w = mpfr_sqrt (MPC_RE (a), y, MPC_RND_RE (rnd));
          inex_t = mpfr_sqrt (MPC_IM (a), y, MPC_RND_IM (rnd));
        }
      else
        {
          mpfr_neg (y, y, GMP_RNDN);
          inex_w = mpfr_sqrt (MPC_RE (a), y, MPC_RND_RE (rnd));
          inex_t = -mpfr_sqrt (MPC_IM (a), y, INV_RND (MPC_RND_IM (rnd)));
          mpfr_neg (MPC_IM (a), MPC_IM (a), GMP_RNDN);
        }
      return MPC_INEX (inex_w, inex_t);
    }

  prec = MPC_MAX_PREC(a);

  mpfr_init (w);
  mpfr_init (t);

  if (re_cmp >= 0)
    {
      rnd_w = MPC_RND_RE (rnd);
      prec_w = MPFR_PREC (MPC_RE (a));
      rnd_t = MPC_RND_IM(rnd);
      prec_t = MPFR_PREC (MPC_IM (a));
    }
  else
    {
      rnd_w = MPC_RND_IM(rnd);
      prec_w = MPFR_PREC (MPC_IM (a));
      rnd_t = MPC_RND_RE(rnd);
      prec_t = MPFR_PREC (MPC_RE (a));
      if (im_cmp < 0)
        {
          rnd_w = INV_RND(rnd_w);
          rnd_t = INV_RND(rnd_t);
        }
    }

  do
    {
      loops ++;
      prec += (loops <= 2) ? mpc_ceil_log2 (prec) + 4 : prec / 2;
      mpfr_set_prec (w, prec);
      mpfr_set_prec (t, prec);
      /* let b = x + iy */
      /* w = sqrt ((|x| + sqrt (x^2 + y^2)) / 2), rounded down */
      /* total error bounded by 3 ulps */
      inex_w = mpc_abs (w, b, GMP_RNDD);
      if (re_cmp < 0)
        inex_w |= mpfr_sub (w, w, MPC_RE (b), GMP_RNDD);
      else
        inex_w |= mpfr_add (w, w, MPC_RE (b), GMP_RNDD);
      inex_w |= mpfr_div_2ui (w, w, 1, GMP_RNDD);
      inex_w |= mpfr_sqrt (w, w, GMP_RNDD);

      ok_w = mpfr_can_round (w, (mp_exp_t) prec - 2, GMP_RNDD, GMP_RNDU,
                             prec_w + (rnd_w == GMP_RNDN));
      if (!inex_w || ok_w)
        {
          /* t = y / 2w, rounded away */
          /* total error bounded by 7 ulps */
          const mp_rnd_t r = im_sgn ? GMP_RNDD : GMP_RNDU;
          inex_t  = mpfr_div (t, MPC_IM (b), w, r);
          inex_t |= mpfr_div_2ui (t, t, 1, r);
          ok_t = mpfr_can_round (t, (mp_exp_t) prec - 3, r, GMP_RNDZ,
                                 prec_t + (rnd_t == GMP_RNDN));
          /* As for w; since t was rounded away, we check whether rounding to 0
             is possible. */
        }
    }
    while ((inex_w && !ok_w) || (inex_t && !ok_t));

  if (re_cmp > 0)
      inex = MPC_INEX (mpfr_set (MPC_RE (a), w, MPC_RND_RE(rnd)),
                       mpfr_set (MPC_IM (a), t, MPC_RND_IM(rnd)));
  else if (im_cmp > 0)
      inex = MPC_INEX (mpfr_set (MPC_RE(a), t, MPC_RND_RE(rnd)),
                       mpfr_set (MPC_IM(a), w, MPC_RND_IM(rnd)));
  else
      inex = MPC_INEX (mpfr_neg (MPC_RE (a), t, MPC_RND_RE(rnd)),
                       mpfr_neg (MPC_IM (a), w, MPC_RND_IM(rnd)));

  mpfr_clear (w);
  mpfr_clear (t);

  return inex;
}
