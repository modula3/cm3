/* File for generic tests.

Copyright (C) 2008, 2009 Philippe Th\'eveny, Andreas Enge, Paul Zimmermann

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

/* #include <stdlib.h> */

#include "mpc-tests.h"

/* Warning: unlike the MPFR macro (defined in mpfr-impl.h), this one returns
   true when b is singular */
#define MPFR_CAN_ROUND(b,err,prec,rnd)                                  \
  (mpfr_zero_p (b) || mpfr_inf_p (b)                                    \
   || mpfr_can_round (b, (long)mpfr_get_prec (b) - (err), (rnd),        \
                      GMP_RNDZ, (prec) + ((rnd)==GMP_RNDN)))

static void
tgeneric_cc (mpc_function *function, mpc_ptr op, mpc_ptr rop,
             mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  /* We compute the result with four times the precision and check whether the
     rounding is correct. Error reports in this part of the algorithm might
     still be wrong, though, since there are two consecutive roundings (but we
     try to avoid them).  */
  function->pointer.CC (rop4, op, rnd);
  function->pointer.CC (rop, op, rnd);

  /* can't use the mpfr_can_round function when argument is singular,
     use a custom macro instead. */
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1, MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1, MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    /* avoid double rounding error */
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  /* rounding failed */
  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op);

  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_fc (mpc_function *function, mpc_ptr op, mpfr_ptr rop,
             mpfr_ptr rop4, mpfr_ptr rop4rnd, mpfr_rnd_t rnd)
{
  function->pointer.FC (rop4, op, rnd);
  function->pointer.FC (rop, op, rnd);
  if (MPFR_CAN_ROUND (rop4, 1, MPFR_PREC (rop), rnd))
    mpfr_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpfr_value (rop, rop4rnd, 1))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op);
  printf ("with rounding mode %s", mpfr_print_rnd_mode (rnd));

  printf ("\n%s                     gives ", function->name);
  MPFR_OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  MPFR_OUT (rop4);
  printf ("and is rounded to                  ");
  MPFR_OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_cfc (mpc_function *function, mpfr_ptr op1, mpc_ptr op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CFC (rop4, op1, op2, rnd);
  function->pointer.CFC (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  MPFR_OUT (op1);
  OUT (op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_ccf (mpc_function *function, mpc_ptr op1, mpfr_ptr op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CCF (rop4, op1, op2, rnd);
  function->pointer.CCF (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op1);
  MPFR_OUT (op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_ccc (mpc_function *function, mpc_ptr op1, mpc_ptr op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  /* We compute the result with four times the precision and check whether the
     rounding is correct. Error reports in this part of the algorithm might
     still be wrong, though, since there are two consecutive roundings (but we
     try to avoid them).  */
  function->pointer.CCC (rop4, op1, op2, rnd);
  function->pointer.CCC (rop, op1, op2, rnd);

  /* can't use mpfr_can_round when argument is singular */
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1, MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1, MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    /* avoid double rounding error */
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  /* rounding failed */
  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op1);
  OUT (op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_ccu (mpc_function *function, mpc_ptr op1, unsigned long int op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CCU (rop4, op1, op2, rnd);
  function->pointer.CCU (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op1);
  printf ("op2=%lu\n", op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_cuc (mpc_function *function, unsigned long int op1, mpc_ptr op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CUC (rop4, op1, op2, rnd);
  function->pointer.CUC (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  printf ("op1=%lu\n", op1);
  OUT (op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_ccs (mpc_function *function, mpc_ptr op1, long int op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CCS (rop4, op1, op2, rnd);
  function->pointer.CCS (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op1);
  printf ("op2=%ld\n", op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}


static void
tgeneric_cci (mpc_function *function, mpc_ptr op1, int op2,
              mpc_ptr rop, mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CCI (rop4, op1, op2, rnd);
  function->pointer.CCI (rop, op1, op2, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  OUT (op1);
  printf ("op2=%d\n", op2);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}

static void
tgeneric_cuuc (mpc_function *function, unsigned long int op1,
               unsigned long int op2, mpc_ptr op3, mpc_ptr rop,
               mpc_ptr rop4, mpc_ptr rop4rnd, mpc_rnd_t rnd)
{
  known_signs_t ks = {1, 1};

  function->pointer.CUUC (rop4, op1, op2, op3, rnd);
  function->pointer.CUUC (rop, op1, op2, op3, rnd);
  if (MPFR_CAN_ROUND (MPC_RE (rop4), 1,  MPFR_PREC (MPC_RE (rop)),
                      MPC_RND_RE (rnd))
      && MPFR_CAN_ROUND (MPC_IM (rop4), 1,  MPFR_PREC (MPC_IM (rop)),
                         MPC_RND_IM (rnd)))
    mpc_set (rop4rnd, rop4, rnd);
  else
    return;

  if (same_mpc_value (rop, rop4rnd, ks))
    return;

  printf ("Rounding in %s might be incorrect for\n", function->name);
  printf ("op1=%lu\n", op1);
  printf ("op2=%lu\n", op2);
  OUT (op3);
  printf ("with rounding mode (%s, %s)",
          mpfr_print_rnd_mode (MPC_RND_RE (rnd)),
          mpfr_print_rnd_mode (MPC_RND_IM (rnd)));

  printf ("\n%s                     gives ", function->name);
  OUT (rop);
  printf ("%s quadruple precision gives ", function->name);
  OUT (rop4);
  printf ("and is rounded to                  ");
  OUT (rop4rnd);

  exit (1);
}


/* Test parameter reuse: the function should not use its output parameter in
   internal computations. */
static void
reuse_cc (mpc_function* function, mpc_srcptr z, mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CC (expected, z, MPC_RNDNN);
  function->pointer.CC (got, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z) for\n", function->name);
      OUT (z);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_fc (mpc_function* function, mpc_ptr z, mpc_ptr x, mpfr_ptr expected)
{
  mpc_set (x, z, MPC_RNDNN); /* exact */
  function->pointer.FC (expected, z, GMP_RNDN);
  function->pointer.FC (MPC_RE (x), x, GMP_RNDN);
  if (!same_mpfr_value (MPC_RE (x), expected, 1))
    {
      mpfr_t got;
      got[0] = MPC_RE(x)[0]; /* display sensible name */
      printf ("Error for %s(MPC_RE(z), z) for\n", function->name);
      OUT (z);
      MPFR_OUT (expected);
      MPFR_OUT (got);

      exit (1);
    }
  mpc_set (x, z, MPC_RNDNN); /* exact */
  function->pointer.FC (MPC_IM (x), x, GMP_RNDN);
  if (!same_mpfr_value (MPC_IM (x), expected, 1))
    {
      mpfr_t got;
      got[0] = MPC_IM(x)[0]; /* display sensible name */
      printf ("Error for %s(MPC_IM(z), z) for \n", function->name);
      OUT (z);
      MPFR_OUT (expected);
      MPFR_OUT (got);

      exit (1);
    }
}

static void
reuse_cfc (mpc_function* function, mpc_srcptr z, mpfr_srcptr x, mpc_ptr got,
           mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CFC (expected, x, z, MPC_RNDNN);
  function->pointer.CFC (got, x, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, x, z) for\n", function->name);
      MPFR_OUT (x);
      OUT (z);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_ccf (mpc_function* function, mpc_srcptr z, mpfr_srcptr x, mpc_ptr got,
           mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CCF (expected, z, x, MPC_RNDNN);
  function->pointer.CCF (got, got, x, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z, x) for\n", function->name);
      OUT (z);
      MPFR_OUT (x);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_ccc (mpc_function* function, mpc_srcptr z, mpc_srcptr x,
           mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CCC (expected, z, x, MPC_RNDNN);
  function->pointer.CCC (got, got, x, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z, x) for\n", function->name);
      OUT (z);
      OUT (x);
      OUT (expected);
      OUT (got);

      exit (1);
    }
  mpc_set (got, x, MPC_RNDNN); /* exact */
  function->pointer.CCC (expected, z, x, MPC_RNDNN);
  function->pointer.CCC (got, z, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(x, z, x) for\n", function->name);
      OUT (z);
      OUT (x);
      OUT (expected);
      OUT (got);

      exit (1);
    }
  mpc_set (got, x, MPC_RNDNN); /* exact */
  function->pointer.CCC (expected, x, x, MPC_RNDNN);
  function->pointer.CCC (got, got, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(x, x, x) for\n", function->name);
      OUT (x);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_ccu (mpc_function* function, mpc_srcptr z, unsigned long ul,
           mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CCU (expected, z, ul, MPC_RNDNN);
  function->pointer.CCU (got, got, ul, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z, n) for\n", function->name);
      OUT (z);
      printf ("n=%lu\n", ul);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_cuc (mpc_function* function, unsigned long ul, mpc_srcptr z,
           mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CUC (expected, ul, z,MPC_RNDNN);
  function->pointer.CUC (got, ul, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, n, z) for\n", function->name);
      printf ("n=%lu\n", ul);
      OUT (z);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_ccs (mpc_function* function, mpc_srcptr z, long lo,
           mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CCS (expected, z, lo, MPC_RNDNN);
  function->pointer.CCS (got, got, lo, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z, n) for\n", function->name);
      OUT (z);
      printf ("n=%ld\n", lo);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_cci (mpc_function* function, mpc_srcptr z, int i,
           mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CCI (expected, z, i, MPC_RNDNN);
  function->pointer.CCI (got, got, i, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, z, n) for\n", function->name);
      OUT (z);
      printf ("n=%d\n", i);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}

static void
reuse_cuuc (mpc_function* function, unsigned long ul1, unsigned long ul2,
            mpc_srcptr z, mpc_ptr got, mpc_ptr expected)
{
  known_signs_t ks = {1, 1};

  mpc_set (got, z, MPC_RNDNN); /* exact */
  function->pointer.CUUC (expected, ul1, ul2, z,MPC_RNDNN);
  function->pointer.CUUC (got, ul1, ul2, got, MPC_RNDNN);
  if (!same_mpc_value (got, expected, ks))
    {
      printf ("Error for %s(z, m, n, z) for\n", function->name);
      printf ("m=%lu\n", ul1);
      printf ("n=%lu\n", ul2);
      OUT (z);
      OUT (expected);
      OUT (got);

      exit (1);
    }
}


/* tgeneric(prec_min, prec_max, step, exp_max) checks rounding with random
   numbers:
   - with precision ranging from prec_min to prec_max with an increment of
   step,
   - with exponent between -exp_max and exp_max.

   It also checks parameter reuse (it is assumed here that either two mpc_t
   variables are equal or they are different, in the sense that the real part
   of one of them cannot be the imaginary part of the other). */
void
tgeneric (mpc_function function, mpfr_prec_t prec_min,
          mpfr_prec_t prec_max, mpfr_prec_t step, mp_exp_t exp_max)
{
  unsigned long ul1 = 0, ul2 = 0;
  long lo = 0;
  int i = 0;
  mpfr_t x1, x2, xxxx;
  mpc_t  z1, z2, z3, z4, zzzz;

  mpfr_rnd_t rnd_re;
  mpfr_rnd_t rnd_im;
  mpfr_prec_t prec;
  mp_exp_t exp_min;
  int special, special_cases;

  mpc_init2 (z1, prec_max);
  switch (function.type)
    {
    case CCC:
      mpc_init2 (z2, prec_max);
      mpc_init2 (z3, prec_max);
      mpc_init2 (z4, prec_max);
      mpc_init2 (zzzz, 4*prec_max);
      special_cases = 4;
      break;
    case FC:
      mpfr_init2 (x1, prec_max);
      mpfr_init2 (x2, prec_max);
      mpfr_init2 (xxxx, 4*prec_max);
      mpc_init2 (z2, prec_max);
      special_cases = 2;
      break;
    case CCF: case CFC:
      mpfr_init2 (x1, prec_max);
      mpc_init2 (z2, prec_max);
      mpc_init2 (z3, prec_max);
      mpc_init2 (zzzz, 4*prec_max);
      special_cases = 3;
      break;
    case CCI: case CCS:
    case CCU: case CUC:
      mpc_init2 (z2, prec_max);
      mpc_init2 (z3, prec_max);
      mpc_init2 (zzzz, 4*prec_max);
      special_cases = 3;
      break;
    case CUUC:
      mpc_init2 (z2, prec_max);
      mpc_init2 (z3, prec_max);
      mpc_init2 (zzzz, 4*prec_max);
      special_cases = 2;
      break;
    case CC:
    default:
      mpc_init2 (z2, prec_max);
      mpc_init2 (z3, prec_max);
      mpc_init2 (zzzz, 4*prec_max);
      special_cases = 2;
    }

  exp_min = mpfr_get_emin ();
  if (exp_max <= 0)
    exp_max = mpfr_get_emax ();
  else if (exp_max > mpfr_get_emax ())
    exp_max = mpfr_get_emax();
  if (-exp_max > exp_min)
    exp_min = - exp_max;

  if (step < 1)
    step = 1;

  for (prec = prec_min, special = 0;
       prec <= prec_max && special <= special_cases;
       prec+=step, special += (prec == prec_max ? 1 : 0))
    {
      /* when prec == prec_max, test functions with pure real/pure imaginary
         parameters */

      /* probability of one zero part in 256th (25 is almost 10%) */
      const unsigned int zero_probability = special != 0 ? 0 : 25;

      mpc_set_prec (z1, prec);
      test_default_random (z1, exp_min, exp_max, 128, zero_probability);

      switch (function.type)
        {
        case CCC:
          mpc_set_prec (z2, prec);
          test_default_random (z2, exp_min, exp_max, 128, zero_probability);
          mpc_set_prec (z3, prec);
          mpc_set_prec (z4, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              mpfr_set_ui (MPC_RE (z2), 0, GMP_RNDN);
              break;
            case 4:
              mpfr_set_ui (MPC_IM (z2), 0, GMP_RNDN);
              break;
            }
          break;
        case FC:
          mpc_set_prec (z2, prec);
          mpfr_set_prec (x1, prec);
          mpfr_set_prec (x2, prec);
          mpfr_set_prec (xxxx, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            }
          break;
        case CCU: case CUC:
          mpc_set_prec (z2, 128);
          do {
            test_default_random (z2, 0, 64, 128, zero_probability);
          } while (!mpfr_fits_ulong_p (MPC_RE (z2), GMP_RNDN));
          ul1 = mpfr_get_ui (MPC_RE(z2), GMP_RNDN);
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              ul1 = 0;
              break;
            }
          break;
        case CUUC:
          mpc_set_prec (z2, 128);
          do {
            test_default_random (z2, 0, 64, 128, zero_probability);
          } while (!mpfr_fits_ulong_p (MPC_RE (z2), GMP_RNDN)
                   ||!mpfr_fits_ulong_p (MPC_IM (z2), GMP_RNDN));
          ul1 = mpfr_get_ui (MPC_RE(z2), GMP_RNDN);
          ul2 = mpfr_get_ui (MPC_IM(z2), GMP_RNDN);
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              ul1 = 0;
              break;
            case 4:
              ul2 = 0;
              break;
            }
          break;
        case CCS:
          mpc_set_prec (z2, 128);
          do {
            test_default_random (z2, 0, 64, 128, zero_probability);
          } while (!mpfr_fits_slong_p (MPC_RE (z2), GMP_RNDN));
          lo = mpfr_get_si (MPC_RE(z2), GMP_RNDN);
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              lo = 0;
              break;
            }
          break;
        case CCI:
          mpc_set_prec (z2, 128);
          do {
            test_default_random (z2, 0, 64, 128, zero_probability);
          } while (!mpfr_fits_slong_p (MPC_RE (z2), GMP_RNDN));
          i = (int)mpfr_get_si (MPC_RE(z2), GMP_RNDN);
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              i = 0;
              break;
            }
          break;
        case CCF: case CFC:
          mpfr_set_prec (x1, prec);
          mpfr_set (x1, MPC_RE (z1), GMP_RNDN);
          test_default_random (z1, exp_min, exp_max, 128, zero_probability);
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            case 3:
              mpfr_set_ui (x1, 0, GMP_RNDN);
              break;
            }
          break;
        case CC:
        default:
          mpc_set_prec (z2, prec);
          mpc_set_prec (z3, prec);
          mpc_set_prec (zzzz, 4*prec);
          switch (special)
            {
            case 1:
              mpfr_set_ui (MPC_RE (z1), 0, GMP_RNDN);
              break;
            case 2:
              mpfr_set_ui (MPC_IM (z1), 0, GMP_RNDN);
              break;
            }
        }

      for (rnd_re = GMP_RNDN; rnd_re < GMP_RND_MAX; ++rnd_re)
        switch (function.type)
          {
          case CCC:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_ccc (&function, z1, z2, z3, zzzz, z4,
                            RNDC (rnd_re, rnd_im));
            reuse_ccc (&function, z1, z2, z3, z4);
            break;
          case FC:
            tgeneric_fc (&function, z1, x1, xxxx, x2, rnd_re);
            reuse_fc (&function, z1, z2, x1);
            break;
          case CC:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_cc (&function, z1, z2, zzzz, z3,
                           RNDC (rnd_re, rnd_im));
            reuse_cc (&function, z1, z2, z3);
            break;
          case CFC:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_cfc (&function, x1, z1, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_cfc (&function, z1, x1, z2, z3);
            break;
          case CCF:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_ccf (&function, z1, x1, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_ccf (&function, z1, x1, z2, z3);
            break;
          case CCU:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_ccu (&function, z1, ul1, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_ccu (&function, z1, ul1, z2, z3);
            break;
          case CUC:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_cuc (&function, ul1, z1, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_cuc (&function, ul1, z1, z2, z3);
            break;
          case CCS:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_ccs (&function, z1, lo, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_ccs (&function, z1, lo, z2, z3);
            break;
          case CCI:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_cci (&function, z1, i, z2, zzzz, z3,
                            RNDC (rnd_re, rnd_im));
            reuse_cci (&function, z1, i, z2, z3);
            break;
          case CUUC:
            for (rnd_im = GMP_RNDN; rnd_im < GMP_RND_MAX; ++rnd_im)
              tgeneric_cuuc (&function, ul1, ul2, z1, z2, zzzz, z3,
                             RNDC (rnd_re, rnd_im));
            reuse_cuuc (&function, ul1, ul2, z1, z2, z3);
            break;
          default:
            printf ("tgeneric not yet implemented for this kind of"
                    "function\n");
            exit (1);
          }
    }

  mpc_clear (z1);
  switch (function.type)
    {
    case CCC:
      mpc_clear (z2);
      mpc_clear (z3);
      mpc_clear (z4);
      mpc_clear (zzzz);
      break;
    case FC:
      mpc_clear (z2);
      mpfr_clear (x1);
      mpfr_clear (x2);
      mpfr_clear (xxxx);
      break;
    case CCF: case CFC:
      mpfr_clear (x1);
      mpc_clear (z2);
      mpc_clear (z3);
      mpc_clear (zzzz);
      break;
    case CUUC:
    case CCI: case CCS:
    case CCU: case CUC:
    case CC:
    default:
      mpc_clear (z2);
      mpc_clear (z3);
      mpc_clear (zzzz);
    }
}
