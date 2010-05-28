/* tsqr -- test file for mpc_sqr.

Copyright (C) 2002, 2005, 2008 Andreas Enge, Paul Zimmermann, Philippe Th\'eveny

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

#include <stdio.h>
#include <stdlib.h>
#include "mpc-tests.h"

void cmpsqr (mpc_srcptr, mpc_rnd_t);
void testsqr (long, long, mp_prec_t, mpc_rnd_t);
void special (void);

void cmpsqr (mpc_srcptr x, mpc_rnd_t rnd)
   /* computes the square of x with the specific function or by simple     */
   /* multiplication using the rounding mode rnd and compares the results  */
   /* and return values.                                                   */
   /* In our current test suite, the real and imaginary parts of x have    */
   /* the same precision, and we use this precision also for the result.   */
   /* Furthermore, we check whether computing the square in the same       */
   /* place yields the same result.                                        */
   /* We also compute the result with four times the precision and check   */
   /* whether the rounding is correct. Error reports in this part of the   */
   /* algorithm might still be wrong, though, since there are two          */
   /* consecutive roundings.                                               */
{
  mpc_t z, t, u;
  int   inexact_z, inexact_t;

  mpc_init2 (z, MPC_MAX_PREC (x));
  mpc_init2 (t, MPC_MAX_PREC (x));
  mpc_init2 (u, 4 * MPC_MAX_PREC (x));

  inexact_z = mpc_sqr (z, x, rnd);
  inexact_t = mpc_mul (t, x, x, rnd);

  if (mpc_cmp (z, t))
    {
      fprintf (stderr, "sqr and mul differ for rnd=(%s,%s) \nx=",
               mpfr_print_rnd_mode(MPC_RND_RE(rnd)),
               mpfr_print_rnd_mode(MPC_RND_IM(rnd)));
      mpc_out_str (stderr, 2, 0, x, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr gives ");
      mpc_out_str (stderr, 2, 0, z, MPC_RNDNN);
      fprintf (stderr, "\nmpc_mul gives ");
      mpc_out_str (stderr, 2, 0, t, MPC_RNDNN);
      fprintf (stderr, "\n");
      exit (1);
    }
  if (inexact_z != inexact_t)
    {
      fprintf (stderr, "The return values of sqr and mul differ for rnd=(%s,%s) \nx=  ",
               mpfr_print_rnd_mode(MPC_RND_RE(rnd)),
               mpfr_print_rnd_mode(MPC_RND_IM(rnd)));
      mpc_out_str (stderr, 2, 0, x, MPC_RNDNN);
      fprintf (stderr, "\nx^2=");
      mpc_out_str (stderr, 2, 0, z, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr gives %i", inexact_z);
      fprintf (stderr, "\nmpc_mul gives %i", inexact_t);
      fprintf (stderr, "\n");
      exit (1);
    }

  mpc_set (t, x, MPC_RNDNN);
  inexact_t = mpc_sqr (t, t, rnd);
  if (mpc_cmp (z, t))
    {
      fprintf (stderr, "sqr and sqr in place differ for rnd=(%s,%s) \nx=",
               mpfr_print_rnd_mode(MPC_RND_RE(rnd)),
               mpfr_print_rnd_mode(MPC_RND_IM(rnd)));
      mpc_out_str (stderr, 2, 0, x, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr          gives ");
      mpc_out_str (stderr, 2, 0, z, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr in place gives ");
      mpc_out_str (stderr, 2, 0, t, MPC_RNDNN);
      fprintf (stderr, "\n");
      exit (1);
    }
  if (inexact_z != inexact_t)
    {
      fprintf (stderr, "The return values of sqr and sqr in place differ for rnd=(%s,%s) \nx=  ",
               mpfr_print_rnd_mode(MPC_RND_RE(rnd)),
               mpfr_print_rnd_mode(MPC_RND_IM(rnd)));
      mpc_out_str (stderr, 2, 0, x, MPC_RNDNN);
      fprintf (stderr, "\nx^2=");
      mpc_out_str (stderr, 2, 0, z, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr          gives %i", inexact_z);
      fprintf (stderr, "\nmpc_sqr in place gives %i", inexact_t);
      fprintf (stderr, "\n");
      exit (1);
    }

  mpc_sqr (u, x, rnd);
  mpc_set (t, u, rnd);
  if (mpc_cmp (z, t))
    {
      fprintf (stderr, "rounding in sqr might be incorrect for rnd=(%s,%s) \nx=",
               mpfr_print_rnd_mode(MPC_RND_RE(rnd)),
               mpfr_print_rnd_mode(MPC_RND_IM(rnd)));
      mpc_out_str (stderr, 2, 0, x, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr                     gives ");
      mpc_out_str (stderr, 2, 0, z, MPC_RNDNN);
      fprintf (stderr, "\nmpc_sqr quadruple precision gives ");
      mpc_out_str (stderr, 2, 0, u, MPC_RNDNN);
      fprintf (stderr, "\nand is rounded to                 ");
      mpc_out_str (stderr, 2, 0, t, MPC_RNDNN);
      fprintf (stderr, "\n");
      exit (1);
    }

  mpc_clear (z);
  mpc_clear (t);
  mpc_clear (u);
}


void
testsqr (long a, long b, mp_prec_t prec, mpc_rnd_t rnd)
{
  mpc_t x;

  mpc_init2 (x, prec);

  mpc_set_si_si (x, a, b, rnd);

  cmpsqr (x, rnd);

  mpc_clear (x);
}


void
special (void)
{
  mpc_t x, z;
  int inexact;

  mpc_init2 (x, 8);
  mpc_init2 (z, 8);

  mpc_set_si_si (x, 4, 3, MPC_RNDNN);
  inexact = mpc_sqr (z, x, MPC_RNDNN);
  if (MPC_INEX_RE(inexact) || MPC_INEX_IM(inexact))
    {
      fprintf (stderr, "Error: (4+3*I)^2 should be exact with prec=8\n");
      exit (1);
    }

  mpc_set_prec (x, 27);
  mpfr_set_str (MPC_RE(x), "1.11111011011000010101000000e-2", 2, GMP_RNDN);
  mpfr_set_str (MPC_IM(x), "1.11010001010110111001110001e-3", 2, GMP_RNDN);

  cmpsqr (x, 0);

  mpc_clear (x);
  mpc_clear (z);
}

void
bugs (void)
{
  mpc_t z1;

  /* reuse bug found by Paul Zimmermann 20081021 */
  mpc_init2 (z1, 2);
  /* RE (z1^2) overflows, IM(z^2) = -0 */
  mpfr_set_str (MPC_RE (z1), "0.11", 2, GMP_RNDN);
  mpfr_mul_2si (MPC_RE (z1), MPC_RE (z1), mpfr_get_emax (), GMP_RNDN);
  mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
  mpc_conj (z1, z1, MPC_RNDNN);
  mpc_sqr (z1, z1, MPC_RNDNN);
  if (!mpfr_inf_p (MPC_RE (z1)) || mpfr_signbit (MPC_RE (z1))
      ||!mpfr_zero_p (MPC_IM (z1)) || !mpfr_signbit (MPC_IM (z1)))
    {
      printf ("Error: Regression, bug 20081021 reproduced\n");
      OUT (z1);
      exit (1);
    }

  mpc_clear (z1);
}

/* infinite loop */
void
bug20090930 (void)
{
  mpc_t rop, op;

  mpc_init2 (rop, 3464);
  mpc_init2 (op, 866);
  mpfr_set_str (MPC_RE(op), "-2.5763c6519ef1510f8afa101a210b8030b1909cc17004db561a25d9b53e2c08c41c01e8bbac5af6299b9d8786030aa14943d841798c8c369287942e4d4cec42a60ab0922af931159805e631128e97f973754ad53972d5d320a651a3b4a667f0ef2b92dbd698d159c3642675140@192158913", 16, GMP_RNDN);
  mpfr_set_str (MPC_IM(op), "-d.15f2d530934dd930d66e89d70762d2337a8f973dd6915eb6b532fd372fcc955df1d852632d4e46fe64154ceda991a1302caf1b0ec622497e3e5724dd05b1c89a06e28d7e18e8af58f5ff4c9998cb31714688867524f41e0b31e847c1bf40de5127f858069998efd7c3e599080@192158893", 16, GMP_RNDN);
  mpc_sqr (rop, op, MPC_RNDNN);
  mpc_clear (rop);
  mpc_clear (op);
}

/* other infinite loop */
void
bug20091001 (void)
{
  mpc_t rop, op;

  mpc_init2 (rop, 2256);
  mpc_init2 (op, 564);
  mpfr_set_str (MPC_RE(op), "c.87999bfd1cb1a64288881e214b7cf1af979863b23c030b79c4a8bebb39177967608388a2e4df527977e7755a25df8af8f72fdd6dd2f42bd00de83088b4e9b59ce85caf2e6b0c0@-184298749", 16, GMP_RNDN);
  mpfr_set_str (MPC_IM(op), "-2.5109af459d4daf357e09475ec991cdc9b02c8f7dfacdc060d2a24710d09c997f8aea6dbd46f10828c30b583fdcc90d7dcbb895689d594d3813db40784d2309e450d1fb6e38da8@-184298726", 16, GMP_RNDN);
  mpc_sqr (rop, op, MPC_RNDNN);
  mpc_clear (rop);
  mpc_clear (op);
}

int
main (void)
{
  DECL_FUNC (CC, f, mpc_sqr);
  test_start ();

  bug20091001 ();
  bug20090930 ();

  special ();

  testsqr (247, -65, 8, 24);
  testsqr (5, -896, 3, 2);
  testsqr (-3, -512, 2, 16);
  testsqr (266013312, 121990769, 27, 0);
  testsqr (170, 9, 8, 0);
  testsqr (768, 85, 8, 16);
  testsqr (145, 1816, 8, 24);
  testsqr (0, 1816, 8, 24);
  testsqr (145, 0, 8, 24);

  data_check (f, "sqr.dat");
  tgeneric (f, 2, 1024, 1, 0);

  bugs ();

  test_end ();

  return 0;
}
