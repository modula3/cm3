/* Test file for in-place operations.

Copyright 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

#define DISP(s, t) {printf(s); mpfr_out_str(stdout, 2, 0, t, GMP_RNDN); }
#define DISP2(s,t) {DISP(s,t); putchar('\n');}

#define SPECIAL_MAX 12

static void
set_special (mpfr_ptr x, unsigned int select)
{
  MPFR_ASSERTN (select < SPECIAL_MAX);
  switch (select)
    {
    case 0:
      MPFR_SET_NAN (x);
      break;
    case 1:
      MPFR_SET_INF (x);
      MPFR_SET_POS (x);
      break;
    case 2:
      MPFR_SET_INF (x);
      MPFR_SET_NEG (x);
      break;
    case 3:
      MPFR_SET_ZERO (x);
      MPFR_SET_POS  (x);
      break;
    case 4:
      MPFR_SET_ZERO (x);
      MPFR_SET_NEG  (x);
      break;
    case 5:
      mpfr_set_str_binary (x, "1");
      break;
    case 6:
      mpfr_set_str_binary (x, "-1");
      break;
    case 7:
      mpfr_set_str_binary (x, "1e-1");
      break;
    case 8:
      mpfr_set_str_binary (x, "1e+1");
      break;
    case 9:
      mpfr_const_pi (x, GMP_RNDN);
      break;
    case 10:
      mpfr_const_pi (x, GMP_RNDN);
      MPFR_SET_EXP (x, MPFR_GET_EXP (x)-1);
      break;
    default:
      mpfr_urandomb (x, RANDS);
      break;
    }
}
/* same than mpfr_cmp, but returns 0 for both NaN's */
static int
mpfr_compare (mpfr_srcptr a, mpfr_srcptr b)
{
  return (MPFR_IS_NAN(a)) ? !MPFR_IS_NAN(b) :
    (MPFR_IS_NAN(b) || mpfr_cmp(a, b));
}

static void
test3 (int (*testfunc)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mp_rnd_t),
       char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref1, ref2, ref3;
  mpfr_t res1;
  int i;

#ifdef DEBUG
  printf("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref2, prec);
  mpfr_init2 (ref3, prec);
  mpfr_init2 (res1, prec);

  /* for each variable, consider each of the following 6 possibilities:
     NaN, +Infinity, -Infinity, +0, -0 or a random number */
  for (i=0; i < SPECIAL_MAX*SPECIAL_MAX ; i++) {
    set_special (ref2, i%SPECIAL_MAX);
    set_special (ref3, i/SPECIAL_MAX);

    /* reference call: foo(a, b, c) */
    testfunc (ref1, ref2, ref3, rnd);

    /* foo(a, a, c) */
    mpfr_set (res1, ref2, rnd); /* exact operation */
    testfunc (res1, res1, ref3, rnd);

    if (mpfr_compare (res1, ref1))
      {
        printf ("Error for %s(a, a, c) for ", foo);
        DISP("a=",ref2); DISP2(", c=",ref3);
        printf ("expected "); mpfr_print_binary (ref1); puts ("");
        printf ("got      "); mpfr_print_binary (res1); puts ("");
        exit (1);
      }

    /* foo(a, b, a) */
    mpfr_set (res1, ref3, rnd);
    testfunc (res1, ref2, res1, rnd);
    if (mpfr_compare (res1, ref1))
      {
        printf ("Error for %s(a, b, a) for ", foo);
        DISP("b=",ref2); DISP2(", a=", ref3);
        DISP("expected ", ref1); DISP2(", got ",res1);
        exit (1);
      }

    /* foo(a, a, a) */
    mpfr_set (ref3, ref2, rnd);
    testfunc (ref1, ref2, ref3, rnd);
    mpfr_set (res1, ref2, rnd);
    testfunc (res1, res1, res1, rnd);

    if (mpfr_compare (res1, ref1))
      {
        printf ("Error for %s(a, a, a) for ", foo);
        DISP2("a=",ref2);
        DISP("expected ", ref1); DISP2(", got", res1);
        exit (1);
      }
  }

  mpfr_clear (ref1);
  mpfr_clear (ref2);
  mpfr_clear (ref3);
  mpfr_clear (res1);
}

static void
test4 (int (*testfunc)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr,
                       mp_rnd_t),
       char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref, op1, op2, op3;
  mpfr_t res;
  int i, j, k;

#ifdef DEBUG
  printf("checking %s\n", foo);
#endif
  mpfr_init2 (ref, prec);
  mpfr_init2 (op1, prec);
  mpfr_init2 (op2, prec);
  mpfr_init2 (op3, prec);
  mpfr_init2 (res, prec);

  /* for each variable, consider each of the following 6 possibilities:
     NaN, +Infinity, -Infinity, +0, -0 or a random number */

  for (i=0; i<SPECIAL_MAX; i++)
    {
      set_special (op1, i);
      for (j=0; j<SPECIAL_MAX; j++)
        {
          set_special (op2, j);
          for (k=0; k<SPECIAL_MAX; k++)
            {
              set_special (op3, k);

              /* reference call: foo(s, a, b, c) */
              testfunc (ref, op1, op2, op3, rnd);

              /* foo(a, a, b, c) */
              mpfr_set (res, op1, rnd); /* exact operation */
              testfunc (res, res, op2, op3, rnd);

              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", b=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo(b, a, b, c) */
              mpfr_set (res, op2, rnd);
              testfunc (res, op1, res, op3, rnd);

              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", b=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo(c, a, b, c) */
              mpfr_set (res, op3, rnd);
              testfunc (res, op1, op2, res, rnd);

              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", b=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo(a, a, a,c) */
              testfunc (ref, op1, op1, op3, rnd);
              mpfr_set (res, op1, rnd);
              testfunc (res, res, res, op3, rnd);
              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", a=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo(a, a, b,a) */
              testfunc (ref, op1, op2, op1, rnd);
              mpfr_set (res, op1, rnd);
              testfunc (res, res, op2, res, rnd);
              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", a=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo(b, a, b, b) */
              testfunc (ref, op1, op2, op2, rnd);
              mpfr_set (res, op2, rnd);
              testfunc (res, op1, res, res, rnd);
              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, b, c) for ", foo);
                  DISP("a=", op1); DISP(", a=", op2); DISP2(", c=", op3);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }

              /* foo (a, a, a, a) */
              testfunc (ref, op1, op1, op1 ,rnd);
              mpfr_set (res, op1, rnd);
              testfunc (res, res, res, res, rnd);
              if (mpfr_compare (res, ref))
                {
                  printf ("Error for %s(a, a, a, a) for ", foo);
                  DISP2("a=", op1);
                  DISP("expected ", ref); DISP2(", got", res);
                  exit (1);
                }
            }
        }
    }

  mpfr_clear (ref);
  mpfr_clear (op1);
  mpfr_clear (op2);
  mpfr_clear (op3);
  mpfr_clear (res);

}

static void
test2ui (int (*testfunc)(mpfr_ptr, mpfr_srcptr, unsigned long int, mp_rnd_t),
         char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref1, ref2;
  unsigned int ref3;
  mpfr_t res1;
  int i;

#ifdef DEBUG
  printf("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref2, prec);
  mpfr_init2 (res1, prec);

  /* ref2 can be NaN, +Inf, -Inf, +0, -0 or any number
     ref3 can be 0 or any number */
  for (i=0; i<SPECIAL_MAX*2; i++)
    {
      set_special (ref2, i%SPECIAL_MAX);
      ref3 = i/SPECIAL_MAX == 0 ? 0 : randlimb ();

      /* reference call: foo(a, b, c) */
      testfunc (ref1, ref2, ref3, rnd);

      /* foo(a, a, c) */
      mpfr_set (res1, ref2, rnd); /* exact operation */
      testfunc (res1, res1, ref3, rnd);

      if (mpfr_compare (res1, ref1))
        {
          printf ("Error for %s(a, a, c) for c=%u\n", foo, ref3);
          DISP2("a=",ref2);
          printf ("expected "); mpfr_print_binary (ref1); puts ("");
          printf ("got      "); mpfr_print_binary (res1); puts ("");
          exit (1);
        }
    }

  mpfr_clear (ref1);
  mpfr_clear (ref2);
  mpfr_clear (res1);
}

static void
testui2 (int (*testfunc)(mpfr_ptr, unsigned long int, mpfr_srcptr, mp_rnd_t),
         char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref1, ref3;
  unsigned int ref2;
  mpfr_t res1;
  int i;

#ifdef DEBUG
  printf("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref3, prec);
  mpfr_init2 (res1, prec);

  for (i=0; i<SPECIAL_MAX*2; i++) {
    set_special (ref3, i%SPECIAL_MAX);
    ref2 = i/SPECIAL_MAX==0 ? 0 : randlimb ();

    /* reference call: foo(a, b, c) */
    testfunc (ref1, ref2, ref3, rnd);

    /* foo(a, b, a) */
    mpfr_set (res1, ref3, rnd); /* exact operation */
    testfunc (res1, ref2, res1, rnd);
    if (mpfr_compare (res1, ref1))
      {
        printf ("Error for %s(a, b, a) for b=%u \n", foo, ref2);
        DISP2("a=", ref3);
        DISP("expected", ref1); DISP2(", got ", res1);
        exit (1);
      }
  }

  mpfr_clear (ref1);
  mpfr_clear (ref3);
  mpfr_clear (res1);
}

/* foo(mpfr_ptr, mpfr_srcptr, mp_rndt) */
static void
test2 (int (*testfunc)(mpfr_ptr, mpfr_srcptr, mp_rnd_t),
       char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref1, ref2;
  mpfr_t res1;
  int i;

#ifdef DEBUG
  printf("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref2, prec);
  mpfr_init2 (res1, prec);

  for (i=0; i<SPECIAL_MAX; i++)
    {
      set_special (ref2, i);

      /* reference call: foo(a, b) */
      testfunc (ref1, ref2, rnd);

      /* foo(a, a) */
      mpfr_set (res1, ref2, rnd); /* exact operation */
      testfunc (res1, res1, rnd);
      if (mpfr_compare (res1, ref1))
        {
          printf ("Error for %s(a, a) for ", foo);
          DISP2("a=", ref2);
          DISP("expected", ref1); DISP2(", got ", res1);
          exit (1);
        }
    }

  mpfr_clear (ref1);
  mpfr_clear (ref2);
  mpfr_clear (res1);
}

/* foo(mpfr_ptr, mpfr_srcptr) */
static void
test2a (int (*testfunc)(mpfr_ptr, mpfr_srcptr),
        char *foo, mp_prec_t prec)
{
  mpfr_t ref1, ref2;
  mpfr_t res1;
  int i;

#ifdef DEBUG
  printf ("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref2, prec);
  mpfr_init2 (res1, prec);

  for (i=0; i<SPECIAL_MAX; i++)
    {
      set_special (ref2, i);

      /* reference call: foo(a, b) */
      testfunc (ref1, ref2);

      /* foo(a, a) */
      mpfr_set (res1, ref2, GMP_RNDN); /* exact operation */
      testfunc (res1, res1);
      if (mpfr_compare (res1, ref1))
        {
          printf ("Error for %s(a, a) for ", foo);
          DISP2("a=",ref2);
          DISP("expected", ref1); DISP2(", got ", res1);
          exit (1);
        }
    }

  mpfr_clear (ref1);
  mpfr_clear (ref2);
  mpfr_clear (res1);
}

/* one operand, two results */
static void
test3a (int (*testfunc)(mpfr_ptr, mpfr_ptr, mpfr_srcptr, mp_rnd_t),
        char *foo, mp_prec_t prec, mp_rnd_t rnd)
{
  mpfr_t ref1, ref2, ref3;
  mpfr_t res1, res2;
  int i;

#ifdef DEBUG
  printf ("checking %s\n", foo);
#endif
  mpfr_init2 (ref1, prec);
  mpfr_init2 (ref2, prec);
  mpfr_init2 (ref3, prec);
  mpfr_init2 (res1, prec);
  mpfr_init2 (res2, prec);

  for (i=0; i<SPECIAL_MAX; i++)
    {
      set_special (ref3, i);

      /* reference call: foo(a, b, c) */
      testfunc (ref1, ref2, ref3, rnd);

      /* foo(a, b, a) */
      mpfr_set (res1, ref3, rnd); /* exact operation */
      testfunc (res1, res2, res1, rnd);
      if (mpfr_compare (res1, ref1) || mpfr_compare (res2, ref2))
        {
          printf ("Error for %s(a, b, a) for rnd=%s, ", foo,
                  mpfr_print_rnd_mode (rnd));
          DISP2("a=",ref3);
          DISP("expected (", ref1); DISP(",",ref2);
          DISP("), got (", res1); DISP(",", res2); printf(")\n");
          exit (1);
        }

      /* foo(a, b, b) */
      mpfr_set (res2, ref3, rnd); /* exact operation */
      testfunc (res1, res2, res2, rnd);
      if (mpfr_compare (res1, ref1) || mpfr_compare (res2, ref2))
        {
          printf ("Error for %s(a, b, b) for ", foo);
          DISP2("b=",ref3);
          DISP("expected (", ref1); DISP(",",ref2);
          DISP("), got (", res1); DISP(",", res2); printf(")\n");
          exit (1);
        }
    }

  mpfr_clear (ref1);
  mpfr_clear (ref2);
  mpfr_clear (ref3);
  mpfr_clear (res1);
  mpfr_clear (res2);
}

static int
reldiff_wrapper (mpfr_ptr a, mpfr_srcptr b, mpfr_srcptr c, mp_rnd_t rnd_mode)
{
  mpfr_reldiff (a, b, c, rnd_mode);
  return 0;
}

int
main (void)
{
  int rnd;
  mp_prec_t p;
  tests_start_mpfr ();

  p = (randlimb () % 200)+ MPFR_PREC_MIN;
  RND_LOOP (rnd)
  {
    test2a (mpfr_round, "mpfr_round", p);
    test2a (mpfr_ceil, "mpfr_ceil", p);
    test2a (mpfr_floor, "mpfr_floor", p);
    test2a (mpfr_trunc, "mpfr_trunc", p);

    test2ui (mpfr_add_ui, "mpfr_add_ui", p, (mp_rnd_t) rnd);
    test2ui (mpfr_div_2exp, "mpfr_div_2exp", p, (mp_rnd_t) rnd);
    test2ui (mpfr_div_ui, "mpfr_div_ui", p, (mp_rnd_t) rnd);
    test2ui (mpfr_mul_2exp, "mpfr_mul_2exp", p, (mp_rnd_t) rnd);
    test2ui (mpfr_mul_ui, "mpfr_mul_ui", p, (mp_rnd_t) rnd);
    test2ui (mpfr_pow_ui, "mpfr_pow_ui", p, (mp_rnd_t) rnd);
    test2ui (mpfr_sub_ui, "mpfr_sub_ui", p, (mp_rnd_t) rnd);

    testui2 (mpfr_ui_div, "mpfr_ui_div", p, (mp_rnd_t) rnd);
    testui2 (mpfr_ui_sub, "mpfr_ui_sub", p, (mp_rnd_t) rnd);
    testui2 (mpfr_ui_pow, "mpfr_ui_pow", p, (mp_rnd_t) rnd);

    test2 (mpfr_sqr, "mpfr_sqr", p, (mp_rnd_t) rnd);
    test2 (mpfr_sqrt, "mpfr_sqrt", p, (mp_rnd_t) rnd);
    test2 (mpfr_abs, "mpfr_abs", p, (mp_rnd_t) rnd);
    test2 (mpfr_neg, "mpfr_neg", p, (mp_rnd_t) rnd);

    test2 (mpfr_log, "mpfr_log", p, (mp_rnd_t) rnd);
    test2 (mpfr_log2, "mpfr_log2", p, (mp_rnd_t) rnd);
    test2 (mpfr_log10, "mpfr_log10", p, (mp_rnd_t) rnd);
    test2 (mpfr_log1p, "mpfr_log1p", p, (mp_rnd_t) rnd);

    test2 (mpfr_exp, "mpfr_exp", p, (mp_rnd_t) rnd);
    test2 (mpfr_exp2, "mpfr_exp2", p, (mp_rnd_t) rnd);
    test2 (mpfr_exp10, "mpfr_exp10", p, (mp_rnd_t) rnd);
    test2 (mpfr_expm1, "mpfr_expm1", p, (mp_rnd_t) rnd);
    test2 (mpfr_eint, "mpfr_eint", p, (mp_rnd_t) rnd);

    test2 (mpfr_sinh, "mpfr_sinh", p, (mp_rnd_t) rnd);
    test2 (mpfr_cosh, "mpfr_cosh", p, (mp_rnd_t) rnd);
    test2 (mpfr_tanh, "mpfr_tanh", p, (mp_rnd_t) rnd);
    test2 (mpfr_asinh, "mpfr_asinh", p, (mp_rnd_t) rnd);
    test2 (mpfr_acosh, "mpfr_acosh", p, (mp_rnd_t) rnd);
    test2 (mpfr_atanh, "mpfr_atanh", p, (mp_rnd_t) rnd);
    test2 (mpfr_sech, "mpfr_sech", p, (mp_rnd_t) rnd);
    test2 (mpfr_csch, "mpfr_csch", p, (mp_rnd_t) rnd);
    test2 (mpfr_coth, "mpfr_coth", p, (mp_rnd_t) rnd);

    test2 (mpfr_asin, "mpfr_asin", p, (mp_rnd_t) rnd);
    test2 (mpfr_acos, "mpfr_acos", p, (mp_rnd_t) rnd);
    test2 (mpfr_atan, "mpfr_atan", p, (mp_rnd_t) rnd);
    test2 (mpfr_cos, "mpfr_cos", p, (mp_rnd_t) rnd);
    test2 (mpfr_sin, "mpfr_sin", p, (mp_rnd_t) rnd);
    test2 (mpfr_tan, "mpfr_tan", p, (mp_rnd_t) rnd);
    test2 (mpfr_sec, "mpfr_sec", p, (mp_rnd_t) rnd);
    test2 (mpfr_csc, "mpfr_csc", p, (mp_rnd_t) rnd);
    test2 (mpfr_cot, "mpfr_cot", p, (mp_rnd_t) rnd);

    test2 (mpfr_erf,  "mpfr_erf",  p, (mp_rnd_t) rnd);
    test2 (mpfr_erfc, "mpfr_erfc", p, (mp_rnd_t) rnd);
    test2 (mpfr_j0,   "mpfr_j0",   p, (mp_rnd_t) rnd);
    test2 (mpfr_j1,   "mpfr_j1",   p, (mp_rnd_t) rnd);
    test2 (mpfr_y0,   "mpfr_y0",   p, (mp_rnd_t) rnd);
    test2 (mpfr_y1,   "mpfr_y1",   p, (mp_rnd_t) rnd);
    test2 (mpfr_zeta, "mpfr_zeta", p, (mp_rnd_t) rnd);
    test2 (mpfr_gamma, "mpfr_gamma", p, (mp_rnd_t) rnd);
    test2 (mpfr_lngamma, "mpfr_lngamma", p, (mp_rnd_t) rnd);

    test2 (mpfr_rint, "mpfr_rint", p, (mp_rnd_t) rnd);
    test2 (mpfr_rint_ceil, "mpfr_rint_ceil", p, (mp_rnd_t) rnd);
    test2 (mpfr_rint_floor, "mpfr_rint_floor", p, (mp_rnd_t) rnd);
    test2 (mpfr_rint_round, "mpfr_rint_round", p, (mp_rnd_t) rnd);
    test2 (mpfr_rint_trunc, "mpfr_rint_trunc", p, (mp_rnd_t) rnd);
    test2 (mpfr_frac, "mpfr_frac", p, (mp_rnd_t) rnd);

    test3 (mpfr_add, "mpfr_add", p, (mp_rnd_t) rnd);
    test3 (mpfr_sub, "mpfr_sub", p, (mp_rnd_t) rnd);
    test3 (mpfr_mul, "mpfr_mul", p, (mp_rnd_t) rnd);
    test3 (mpfr_div, "mpfr_div", p, (mp_rnd_t) rnd);

    test3 (mpfr_agm, "mpfr_agm", p, (mp_rnd_t) rnd);
    test3 (mpfr_min, "mpfr_min", p, (mp_rnd_t) rnd);
    test3 (mpfr_max, "mpfr_max", p, (mp_rnd_t) rnd);

    test3 (reldiff_wrapper, "mpfr_reldiff", p, (mp_rnd_t) rnd);
    test3 (mpfr_dim, "mpfr_dim", p, (mp_rnd_t) rnd);

    test3 (mpfr_remainder, "mpfr_remainder", p, (mp_rnd_t) rnd);
    test3 (mpfr_pow, "mpfr_pow", p, (mp_rnd_t) rnd);
    test3 (mpfr_atan2, "mpfr_atan2", p, (mp_rnd_t) rnd);
    test3 (mpfr_hypot, "mpfr_hypot", p, (mp_rnd_t) rnd);

    test3a (mpfr_sin_cos, "mpfr_sin_cos", p, (mp_rnd_t) rnd);

    test4 (mpfr_fma, "mpfr_fma", p, (mp_rnd_t) rnd);
    test4 (mpfr_fms, "mpfr_fms", p, (mp_rnd_t) rnd);

#if MPFR_VERSION >= MPFR_VERSION_NUM(2,4,0)
    test2 (mpfr_li2, "mpfr_li2",  p, (mp_rnd_t) rnd);
    test2 (mpfr_rec_sqrt, "mpfr_rec_sqrt",  p, (mp_rnd_t) rnd);
    test3 (mpfr_fmod, "mpfr_fmod", p, (mp_rnd_t) rnd);
    test3a (mpfr_modf, "mpfr_modf", p, (mp_rnd_t) rnd);
    test3a (mpfr_sinh_cosh, "mpfr_sinh_cosh", p, (mp_rnd_t) rnd);
#endif
  }

  tests_end_mpfr ();
  return 0;
}
