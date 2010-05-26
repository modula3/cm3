/* Test file for mpfr_mul.

Copyright 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

#ifdef CHECK_EXTERNAL
static int
test_mul (mpfr_ptr a, mpfr_srcptr b, mpfr_srcptr c, mp_rnd_t rnd_mode)
{
  int res;
  int ok = rnd_mode == GMP_RNDN && mpfr_number_p (b) && mpfr_number_p (c);
  if (ok)
    {
      mpfr_print_raw (b);
      printf (" ");
      mpfr_print_raw (c);
    }
  res = mpfr_mul (a, b, c, rnd_mode);
  if (ok)
    {
      printf (" ");
      mpfr_print_raw (a);
      printf ("\n");
    }
  return res;
}
#else
#define test_mul mpfr_mul
#endif

/* Workaround for sparc gcc 2.95.x bug, see notes in tadd.c. */
#define check(x,y,rnd_mode,px,py,pz,res)  pcheck(x,y,res,rnd_mode,px,py,pz)

/* checks that x*y gives the right result */
static void
pcheck (const char *xs, const char *ys, const char *res, mp_rnd_t rnd_mode,
        unsigned int px, unsigned int py, unsigned int pz)
{
  mpfr_t xx, yy, zz;

  mpfr_init2 (xx, px);
  mpfr_init2 (yy, py);
  mpfr_init2 (zz, pz);
  mpfr_set_str1 (xx, xs);
  mpfr_set_str1 (yy, ys);
  test_mul(zz, xx, yy, rnd_mode);
  if (mpfr_cmp_str1 (zz, res) )
    {
      printf ("(1)mpfr_mul failed for x=%s y=%s with rnd=%s\n",
              xs, ys, mpfr_print_rnd_mode (rnd_mode));
      printf ("correct is %s, mpfr_mul gives ", res);
      mpfr_out_str(stdout, 10, 0, zz, GMP_RNDN);
      /*
        printf("\nBinary forms:\nxx=");
        mpfr_print_binary (xx);
        printf("\nyy=");
        mpfr_print_binary (yy);
        printf("\nzz=");
        mpfr_print_binary(zz);
        printf("\nre=");
        mpfr_set_str1 (zz, res);
        mpfr_print_binary(zz);
        putchar('\n');*/
      exit (1);
    }
  mpfr_clear(xx); mpfr_clear(yy); mpfr_clear(zz);
}

static void
check53 (const char *xs, const char *ys, mp_rnd_t rnd_mode, const char *zs)
{
  mpfr_t xx, yy, zz;

  mpfr_inits2 (53, xx, yy, zz, (mpfr_ptr) 0);
  mpfr_set_str1 (xx, xs);
  mpfr_set_str1 (yy, ys);
  test_mul (zz, xx, yy, rnd_mode);
  if (mpfr_cmp_str1 (zz, zs) )
    {
      printf ("(2) mpfr_mul failed for x=%s y=%s with rnd=%s\n",
              xs, ys, mpfr_print_rnd_mode(rnd_mode));
      printf ("correct result is %s,\n mpfr_mul gives ", zs);
      mpfr_out_str(stdout, 10, 0, zz, GMP_RNDN);
      /*
        printf("\nBinary forms:\nxx=");
        mpfr_print_binary (xx);
        printf("\nyy=");
        mpfr_print_binary (yy);
        printf("\nzz=");
        mpfr_print_binary(zz);
        printf("\nre=");
        mpfr_set_str1 (zz, zs);
        mpfr_print_binary(zz);
        putchar('\n'); */
      exit (1);
    }
  mpfr_clears (xx, yy, zz, (mpfr_ptr) 0);
}

/* checks that x*y gives the right result with 24 bits of precision */
static void
check24 (const char *xs, const char *ys, mp_rnd_t rnd_mode, const char *zs)
{
  mpfr_t xx, yy, zz;

  mpfr_inits2 (24, xx, yy, zz, (mpfr_ptr) 0);
  mpfr_set_str1 (xx, xs);
  mpfr_set_str1 (yy, ys);
  test_mul (zz, xx, yy, rnd_mode);
  if (mpfr_cmp_str1 (zz, zs) )
    {
      printf ("(3) mpfr_mul failed for x=%s y=%s with "
              "rnd=%s\n", xs, ys, mpfr_print_rnd_mode(rnd_mode));
      printf ("correct result is gives %s, mpfr_mul gives ", zs);
      mpfr_out_str(stdout, 10, 0, zz, GMP_RNDN);
      putchar('\n');
      exit (1);
    }
  mpfr_clears (xx, yy, zz, (mpfr_ptr) 0);
}

/* the following examples come from the paper "Number-theoretic Test
   Generation for Directed Rounding" from Michael Parks, Table 1 */
static void
check_float (void)
{
  check24("8388609.0",  "8388609.0", GMP_RNDN, "70368760954880.0");
  check24("16777213.0", "8388609.0", GMP_RNDN, "140737479966720.0");
  check24("8388611.0",  "8388609.0", GMP_RNDN, "70368777732096.0");
  check24("12582911.0", "8388610.0", GMP_RNDN, "105553133043712.0");
  check24("12582914.0", "8388610.0", GMP_RNDN, "105553158209536.0");
  check24("13981013.0", "8388611.0", GMP_RNDN, "117281279442944.0");
  check24("11184811.0", "8388611.0", GMP_RNDN, "93825028587520.0");
  check24("11184810.0", "8388611.0", GMP_RNDN, "93825020198912.0");
  check24("13981014.0", "8388611.0", GMP_RNDN, "117281287831552.0");

  check24("8388609.0",  "8388609.0", GMP_RNDZ, "70368760954880.0");
  check24("16777213.0", "8388609.0", GMP_RNDZ, "140737471578112.0");
  check24("8388611.0",  "8388609.0", GMP_RNDZ, "70368777732096.0");
  check24("12582911.0", "8388610.0", GMP_RNDZ, "105553124655104.0");
  check24("12582914.0", "8388610.0", GMP_RNDZ, "105553158209536.0");
  check24("13981013.0", "8388611.0", GMP_RNDZ, "117281271054336.0");
  check24("11184811.0", "8388611.0", GMP_RNDZ, "93825028587520.0");
  check24("11184810.0", "8388611.0", GMP_RNDZ, "93825011810304.0");
  check24("13981014.0", "8388611.0", GMP_RNDZ, "117281287831552.0");

  check24("8388609.0",  "8388609.0", GMP_RNDU, "70368769343488.0");
  check24("16777213.0", "8388609.0", GMP_RNDU, "140737479966720.0");
  check24("8388611.0",  "8388609.0", GMP_RNDU, "70368786120704.0");
  check24("12582911.0", "8388610.0", GMP_RNDU, "105553133043712.0");
  check24("12582914.0", "8388610.0", GMP_RNDU, "105553166598144.0");
  check24("13981013.0", "8388611.0", GMP_RNDU, "117281279442944.0");
  check24("11184811.0", "8388611.0", GMP_RNDU, "93825036976128.0");
  check24("11184810.0", "8388611.0", GMP_RNDU, "93825020198912.0");
  check24("13981014.0", "8388611.0", GMP_RNDU, "117281296220160.0");

  check24("8388609.0",  "8388609.0", GMP_RNDD, "70368760954880.0");
  check24("16777213.0", "8388609.0", GMP_RNDD, "140737471578112.0");
  check24("8388611.0",  "8388609.0", GMP_RNDD, "70368777732096.0");
  check24("12582911.0", "8388610.0", GMP_RNDD, "105553124655104.0");
  check24("12582914.0", "8388610.0", GMP_RNDD, "105553158209536.0");
  check24("13981013.0", "8388611.0", GMP_RNDD, "117281271054336.0");
  check24("11184811.0", "8388611.0", GMP_RNDD, "93825028587520.0");
  check24("11184810.0", "8388611.0", GMP_RNDD, "93825011810304.0");
  check24("13981014.0", "8388611.0", GMP_RNDD, "117281287831552.0");
}

/* check sign of result */
static void
check_sign (void)
{
  mpfr_t a, b;

  mpfr_init2 (a, 53);
  mpfr_init2 (b, 53);
  mpfr_set_si (a, -1, GMP_RNDN);
  mpfr_set_ui (b, 2, GMP_RNDN);
  test_mul(a, b, b, GMP_RNDN);
  if (mpfr_cmp_ui (a, 4) )
    {
      printf ("2.0*2.0 gives \n");
      mpfr_out_str(stdout, 10, 0, a, GMP_RNDN);
      putchar('\n');
      exit (1);
    }
  mpfr_clear(a); mpfr_clear(b);
}

/* checks that the inexact return value is correct */
static void
check_exact (void)
{
  mpfr_t a, b, c, d;
  mp_prec_t prec;
  int i, inexact;
  mp_rnd_t rnd;

  mpfr_init (a);
  mpfr_init (b);
  mpfr_init (c);
  mpfr_init (d);

  mpfr_set_prec (a, 17);
  mpfr_set_prec (b, 17);
  mpfr_set_prec (c, 32);
  mpfr_set_str_binary (a, "1.1000111011000100e-1");
  mpfr_set_str_binary (b, "1.0010001111100111e-1");
  if (test_mul (c, a, b, GMP_RNDZ))
    {
      printf ("wrong return value (1)\n");
      exit (1);
    }

  for (prec = 2; prec < 100; prec++)
    {
      mpfr_set_prec (a, prec);
      mpfr_set_prec (b, prec);
      mpfr_set_prec (c, 2 * prec - 2);
      mpfr_set_prec (d, 2 * prec);
      for (i = 0; i < 1000; i++)
        {
          mpfr_random (a);
          mpfr_random (b);
          rnd = (mp_rnd_t) RND_RAND ();
          inexact = test_mul (c, a, b, rnd);
          if (test_mul (d, a, b, rnd)) /* should be always exact */
            {
              printf ("unexpected inexact return value\n");
              exit (1);
            }
          if ((inexact == 0) && mpfr_cmp (c, d))
            {
              printf ("inexact=0 but results differ\n");
              exit (1);
            }
          else if (inexact && (mpfr_cmp (c, d) == 0))
            {
              printf ("inexact!=0 but results agree\n");
              printf ("prec=%u rnd=%s a=", (unsigned int) prec,
                      mpfr_print_rnd_mode (rnd));
              mpfr_out_str (stdout, 2, 0, a, rnd);
              printf ("\nb=");
              mpfr_out_str (stdout, 2, 0, b, rnd);
              printf ("\nc=");
              mpfr_out_str (stdout, 2, 0, c, rnd);
              printf ("\nd=");
              mpfr_out_str (stdout, 2, 0, d, rnd);
              printf ("\n");
              exit (1);
            }
        }
    }

  mpfr_clear (a);
  mpfr_clear (b);
  mpfr_clear (c);
  mpfr_clear (d);
}

static void
check_max(void)
{
  mpfr_t xx, yy, zz;
  mp_exp_t emin;

  mpfr_init2(xx, 4);
  mpfr_init2(yy, 4);
  mpfr_init2(zz, 4);
  mpfr_set_str1 (xx, "0.68750");
  mpfr_mul_2si(xx, xx, MPFR_EMAX_DEFAULT/2, GMP_RNDN);
  mpfr_set_str1 (yy, "0.68750");
  mpfr_mul_2si(yy, yy, MPFR_EMAX_DEFAULT - MPFR_EMAX_DEFAULT/2 + 1, GMP_RNDN);
  mpfr_clear_flags();
  test_mul(zz, xx, yy, GMP_RNDU);
  if (!(mpfr_overflow_p() && MPFR_IS_INF(zz)))
    {
      printf("check_max failed (should be an overflow)\n");
      exit(1);
    }

  mpfr_clear_flags();
  test_mul(zz, xx, yy, GMP_RNDD);
  if (mpfr_overflow_p() || MPFR_IS_INF(zz))
    {
      printf("check_max failed (should NOT be an overflow)\n");
      exit(1);
    }
  mpfr_set_str1 (xx, "0.93750");
  mpfr_mul_2si(xx, xx, MPFR_EMAX_DEFAULT, GMP_RNDN);
  if (!(MPFR_IS_FP(xx) && MPFR_IS_FP(zz)))
    {
      printf("check_max failed (internal error)\n");
      exit(1);
    }
  if (mpfr_cmp(xx, zz) != 0)
    {
      printf("check_max failed: got ");
      mpfr_out_str(stdout, 2, 0, zz, GMP_RNDZ);
      printf(" instead of ");
      mpfr_out_str(stdout, 2, 0, xx, GMP_RNDZ);
      printf("\n");
      exit(1);
    }

  /* check underflow */
  emin = mpfr_get_emin ();
  set_emin (0);
  mpfr_set_str_binary (xx, "0.1E0");
  mpfr_set_str_binary (yy, "0.1E0");
  test_mul (zz, xx, yy, GMP_RNDN);
  /* exact result is 0.1E-1, which should round to 0 */
  MPFR_ASSERTN(mpfr_cmp_ui (zz, 0) == 0 && MPFR_IS_POS(zz));
  set_emin (emin);

  /* coverage test for mpfr_powerof2_raw */
  emin = mpfr_get_emin ();
  set_emin (0);
  mpfr_set_prec (xx, mp_bits_per_limb + 1);
  mpfr_set_str_binary (xx, "0.1E0");
  mpfr_nextabove (xx);
  mpfr_set_str_binary (yy, "0.1E0");
  test_mul (zz, xx, yy, GMP_RNDN);
  /* exact result is just above 0.1E-1, which should round to minfloat */
  MPFR_ASSERTN(mpfr_cmp (zz, yy) == 0);
  set_emin (emin);

  mpfr_clear(xx);
  mpfr_clear(yy);
  mpfr_clear(zz);
}

static void
check_min(void)
{
  mpfr_t xx, yy, zz;

  mpfr_init2(xx, 4);
  mpfr_init2(yy, 4);
  mpfr_init2(zz, 3);
  mpfr_set_str1(xx, "0.9375");
  mpfr_mul_2si(xx, xx, MPFR_EMIN_DEFAULT/2, GMP_RNDN);
  mpfr_set_str1(yy, "0.9375");
  mpfr_mul_2si(yy, yy, MPFR_EMIN_DEFAULT - MPFR_EMIN_DEFAULT/2 - 1, GMP_RNDN);
  test_mul(zz, xx, yy, GMP_RNDD);
  if (mpfr_sgn(zz) != 0)
    {
      printf("check_min failed: got ");
      mpfr_out_str(stdout, 2, 0, zz, GMP_RNDZ);
      printf(" instead of 0\n");
      exit(1);
    }

  test_mul(zz, xx, yy, GMP_RNDU);
  mpfr_set_str1 (xx, "0.5");
  mpfr_mul_2si(xx, xx, MPFR_EMIN_DEFAULT, GMP_RNDN);
  if (mpfr_sgn(xx) <= 0)
    {
      printf("check_min failed (internal error)\n");
      exit(1);
    }
  if (mpfr_cmp(xx, zz) != 0)
    {
      printf("check_min failed: got ");
      mpfr_out_str(stdout, 2, 0, zz, GMP_RNDZ);
      printf(" instead of ");
      mpfr_out_str(stdout, 2, 0, xx, GMP_RNDZ);
      printf("\n");
      exit(1);
    }

  mpfr_clear(xx);
  mpfr_clear(yy);
  mpfr_clear(zz);
}

static void
check_nans (void)
{
  mpfr_t  p, x, y;

  mpfr_init2 (x, 123L);
  mpfr_init2 (y, 123L);
  mpfr_init2 (p, 123L);

  /* nan * 0 == nan */
  mpfr_set_nan (x);
  mpfr_set_ui (y, 0L, GMP_RNDN);
  test_mul (p, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (p));

  /* 1 * nan == nan */
  mpfr_set_ui (x, 1L, GMP_RNDN);
  mpfr_set_nan (y);
  test_mul (p, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (p));

  /* 0 * +inf == nan */
  mpfr_set_ui (x, 0L, GMP_RNDN);
  mpfr_set_nan (y);
  test_mul (p, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (p));

  /* +1 * +inf == +inf */
  mpfr_set_ui (x, 1L, GMP_RNDN);
  mpfr_set_inf (y, 1);
  test_mul (p, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (p));
  MPFR_ASSERTN (mpfr_sgn (p) > 0);

  /* -1 * +inf == -inf */
  mpfr_set_si (x, -1L, GMP_RNDN);
  mpfr_set_inf (y, 1);
  test_mul (p, x, y, GMP_RNDN);
  MPFR_ASSERTN (mpfr_inf_p (p));
  MPFR_ASSERTN (mpfr_sgn (p) < 0);

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (p);
}

static void
check_regression (void)
{
  mpfr_t x, y, z;
  int i;

  mpfr_inits2 (6177, x, y, z, (mpfr_ptr) 0);

  mpfr_set_str (y,
"5.17cc1b727220a94fe13abe8fa9a6ee06db14acc9e21c820ff28b1d5ef5de2b0db92371d212"
"6e9700324977504e8c90e7f0ef58e5894d39f74411afa975da24274ce38135a2fbf209cc8eb1"
"cc1a99cfa4e422fc5defc941d8ffc4bffef02cc07f79788c5ad05368fb69b3f6793e584dba7a"
"31fb34f2ff516ba93dd63f5f2f8bd9e839cfbc529497535fdafd88fc6ae842b0198237e3db5d"
"5f867de104d7a1b0ed4f1c8b0af730d8432ccc2af8a50342046ffec4026b9939883030aab653"
"9d464b0713de04635a3e20ce1b3e6ee74049541ace23b45cb0e536ed7a268ab8c829f52ff838"
"29fbf19f419616f27cc193edde19e9377b58f2f7c4f9d0f9ae5793f8ec3f890c83e3e12357d3"
"76abb9698219d8ae30a5ace8ce1e16256a0a6962e8006233ec316b8f1cd634d803119be695a4"
"bd3da6aaa9bfb1f6b8c0851fe3b26954eb255ebb87c3e31abd83d738a8bab24e06ceb1d9c425"
"3e591923bc56b11aa2d5c8f800d8578efe70cff98cfb50f3330abcca3fdd66c3fbf5bb29144f"
"419305ff366e277849b366a1faeebef0b6f1dac494def14116974431426ac711965630b71846"
"5bef028600bd38ef9adf00c1a099731094180a441adc77abfd856f9748f21a52469b3886c6ed"
"5212fd76730b55214055a4ce9f953033fbbae41e151c41e30bc39c52d4657deebb7b1d316e5d"
"ffa77c0c6b3e09322e52a9b6ce569541446b0e13be4890a13024da309622ce22262e448d926f"
"98b8056a1ea72a494886afefe5f00664a0f7767387a9f09c078f661f3d9947c63ca02c99f38e"
"0d9849779a285ce09443d9055cfda9761492397993db6aa864853b90ff3b5cb6598a50b3cf13"
"ca0c4effa4bca744273714b98ccb5f6c41b2faf877eddda4d24365233a13938992ec6dc0acf8"
"4f2de1298c69cba7b8e0298008606b40425ac77164855238173ba126b5ed33efbb92437778b4"
"fd34a477b48da28a9e8f9056799cc103f25fab431d92f9f6e81aea03fc4c294a92ae0321b886"
"c369924193aa62dea38a7372a22e08485b4fa956ab30a4e8393a8022eed9dda62bb750bfcc3b"
"eb5a4dd138e94b4cb000000000@1535", 16, GMP_RNDN);
  mpfr_set_str (z,
"3.243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89452821e638"
"d01377be5466cf34e90c6cc0ac29b7c97c50dd3f84d5b5b54709179216d5d98979fb1bd1310b"
"a698dfb5ac2ffd72dbd01adfb7b8e1afed6a267e96ba7c9045f12c7f9924a19947b3916cf708"
"01f2e2858efc16636920d871574e69a458fea3f4933d7e0d95748f728eb658718bcd5882154a"
"ee7b54a41dc25a59b59c30d5392af26013c5d1b023286085f0ca417918b8db38ef8e79dcb060"
"3a180e6c9e0e8bb01e8a3ed71577c1bd314b2778af2fda55605c60e65525f3aa55ab94574898"
"6263e8144055ca396a2aab10b6b4cc5c341141e8cea15486af7c72e993b3ee1411636fbc2a2b"
"a9c55d741831f6ce5c3e169b87931eafd6ba336c24cf5c7a325381289586773b8f48986b4bb9"
"afc4bfe81b6628219361d809ccfb21a991487cac605dec8032ef845d5de98575b1dc262302eb"
"651b8823893e81d396acc50f6d6ff383f442392e0b4482a484200469c8f04a9e1f9b5e21c668"
"42f6e96c9a670c9c61abd388f06a51a0d2d8542f68960fa728ab5133a36eef0b6c137a3be4ba"
"3bf0507efb2a98a1f1651d39af017666ca593e82430e888cee8619456f9fb47d84a5c33b8b5e"
"bee06f75d885c12073401a449f56c16aa64ed3aa62363f77061bfedf72429b023d37d0d724d0"
"0a1248db0fead349f1c09b075372c980991b7b25d479d8f6e8def7e3fe501ab6794c3b976ce0"
"bd04c006bac1a94fb6409f60c45e5c9ec2196a246368fb6faf3e6c53b51339b2eb3b52ec6f6d"
"fc511f9b30952ccc814544af5ebd09bee3d004de334afd660f2807192e4bb3c0cba85745c874"
"0fd20b5f39b9d3fbdb5579c0bd1a60320ad6a100c6402c7279679f25fefb1fa3cc8ea5e9f8db"
"3222f83c7516dffd616b152f501ec8ad0552ab323db5fafd23876053317b483e00df829e5c57"
"bbca6f8ca01a87562edf1769dbd542a8f6287effc3ac6732c68c4f5573695b27b0bbca58c8e1"
"ffa35db8f011a010fa3d98fd2183b84afcb56c2dd1d35b9a53e479b6f84565d28e49bc4bfb97"
"90e1ddf2daa4cb7e3362fb1342", 16, GMP_RNDN);
  i = mpfr_mul (x, y, z, GMP_RNDN);
  if (mpfr_cmp_str (x,
 "f.fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
 "fffef09109690@1535",16, GMP_RNDN) != 0 || i != -1)
    {
      printf ("Regression test 1 failed (i=%d, expected -1)\nx=", i);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN); putchar ('\n');
      exit (1);
    }

  mpfr_set_prec (x, 606);
  mpfr_set_prec (y, 606);
  mpfr_set_prec (z, 606);

  mpfr_set_str (y, "-f.ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff92daefc3f8052ca9f58736564d9e93e62d324@-1", 16, GMP_RNDN);
  mpfr_set_str (z, "-f.ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff92daefc3f8052ca9f58736564d9e93e62d324@-1", 16, GMP_RNDN);
  i = mpfr_mul (x, y, z, GMP_RNDU);
  mpfr_set_str (y, "f.ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff25b5df87f00a5953eb0e6cac9b3d27cc5a64c@-1", 16, GMP_RNDN);
  if (mpfr_cmp (x, y) || i <= 0)
    {
      printf ("Regression test (2) failed! (i=%d - Expected 1)\n", i);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN); putchar ('\n');
      exit (1);
    }

  mpfr_set_prec (x, 184);
  mpfr_set_prec (y, 92);
  mpfr_set_prec (z, 1023);

  mpfr_set_str (y, "6.9b8c8498882770d8038c3b0@-1", 16, GMP_RNDN);
  mpfr_set_str (z, "7.44e24b986e7fb296f1e936ce749fec3504cbf0d5ba769466b1c9f1578115efd5d29b4c79271191a920a99280c714d3a657ad6e3afbab77ffce9d697e9bb9110e26d676069afcea8b69f1d1541f2365042d80a97c21dcccd8ace4f1bb58b49922003e738e6f37bb82ef653cb2e87f763974e6ae50ae54e7724c38b80653e3289@255", 16, GMP_RNDN);
  i = mpfr_mul (x, y, z, GMP_RNDU);
  mpfr_set_prec (y, 184);
  mpfr_set_str (y, "3.0080038f2ac5054e3e71ccbb95f76aaab2221715025a28@255",
                16, GMP_RNDN);
  if (mpfr_cmp (x, y) || i <= 0)
    {
      printf ("Regression test (4) failed! (i=%d - expected 1)\n", i);
      printf ("Ref: 3.0080038f2ac5054e3e71ccbb95f76aaab2221715025a28@255\n"
              "Got: ");
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 908);
  mpfr_set_prec (y, 908);
  mpfr_set_prec (z, 908);
  mpfr_set_str (y, "-f.fffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"ffffffffffffffffffffffffffffffffffffffffffffffffffffff99be91f83ec6f0ed28a3d42"
"e6e9a327230345ea6@-1", 16, GMP_RNDN);
  mpfr_set_str (z, "-f.fffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"ffffffffffffffffffffffffffffffffffffffffffffffffffffff99be91f83ec6f0ed28a3d42"
                "e6e9a327230345ea6@-1", 16, GMP_RNDN);
  i = mpfr_mul (x, y, z, GMP_RNDU);
  mpfr_set_str (y, "f.ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
"fffffffffffffffffffffffffffffffffffffffffffffffffffff337d23f07d8de1da5147a85c"
"dd3464e46068bd4d@-1", 16, GMP_RNDN);
  if (mpfr_cmp (x, y) || i <= 0)
    {
      printf ("Regression test (5) failed! (i=%d - expected 1)\n", i);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }


  mpfr_set_prec (x, 50);
  mpfr_set_prec (y, 40);
  mpfr_set_prec (z, 53);
  mpfr_set_str (y, "4.1ffffffff8", 16, GMP_RNDN);
  mpfr_set_str (z, "4.2000000ffe0000@-4", 16, GMP_RNDN);
  i = mpfr_mul (x, y, z, GMP_RNDN);
  if (mpfr_cmp_str (x, "1.104000041d6c0@-3", 16, GMP_RNDN) != 0
      || i <= 0)
    {
      printf ("Regression test (6) failed! (i=%d - expected 1)\nx=", i);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
      printf ("\nMore prec=");
      mpfr_set_prec (x, 93);
      mpfr_mul (x, y, z, GMP_RNDN);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 439);
  mpfr_set_prec (y, 393);
  mpfr_set_str (y, "-1.921fb54442d18469898cc51701b839a252049c1114cf98e804177d"
                "4c76273644a29410f31c6809bbdf2a33679a748636600",
                16, GMP_RNDN);
  i = mpfr_mul (x, y, y, GMP_RNDU);
  if (mpfr_cmp_str (x, "2.77a79937c8bbcb495b89b36602306b1c2159a8ff834288a19a08"
    "84094f1cda3dc426da61174c4544a173de83c2500f8bfea2e0569e3698",
                    16, GMP_RNDN) != 0
      || i <= 0)
    {
      printf ("Regression test (7) failed! (i=%d - expected 1)\nx=", i);
      mpfr_out_str (stdout, 16, 0, x, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_set_prec (x, 1023);
  mpfr_set_prec (y, 1023);
  mpfr_set_prec (z, 511);
  mpfr_set_ui (x, 17, GMP_RNDN);
  mpfr_set_ui (y, 42, GMP_RNDN);
  i = mpfr_mul (z, x, y, GMP_RNDN);
  if (mpfr_cmp_ui (z, 17*42) != 0 || i != 0)
    {
      printf ("Regression test (8) failed! (i=%d - expected 0)\nz=", i);
      mpfr_out_str (stdout, 16, 0, z, GMP_RNDN);
      printf ("\n");
      exit (1);
    }

  mpfr_clears (x, y, z, (mpfr_ptr) 0);
}

#define TEST_FUNCTION test_mul
#define TWO_ARGS
#define RAND_FUNCTION(x) mpfr_random2(x, MPFR_LIMB_SIZE (x), randlimb () % 100)
#include "tgeneric.c"

/* multiplies x by 53-bit approximation of Pi */
static int
mpfr_mulpi (mpfr_t y, mpfr_t x, mp_rnd_t r)
{
  mpfr_t z;
  int inex;

  mpfr_init2 (z, 53);
  mpfr_set_str_binary (z, "11.001001000011111101101010100010001000010110100011");
  inex = mpfr_mul (y, x, z, r);
  mpfr_clear (z);
  return inex;
}

int
main (int argc, char *argv[])
{
  tests_start_mpfr ();

  check_nans ();
  check_exact ();
  check_float ();

  check53("6.9314718055994530941514e-1", "0.0", GMP_RNDZ, "0.0");
  check53("0.0", "6.9314718055994530941514e-1", GMP_RNDZ, "0.0");
  check_sign();
  check53("-4.165000000e4", "-0.00004801920768307322868063274915", GMP_RNDN,
          "2.0");
  check53("2.71331408349172961467e-08", "-6.72658901114033715233e-165",
          GMP_RNDZ, "-1.8251348697787782844e-172");
  check53("0.31869277231188065", "0.88642843322303122", GMP_RNDZ,
          "2.8249833483992453642e-1");
  check("8.47622108205396074254e-01", "3.24039313247872939883e-01", GMP_RNDU,
        28, 45, 2, "0.375");
  check("2.63978122803639081440e-01", "6.8378615379333496093e-1", GMP_RNDN,
        34, 23, 31, "0.180504585267044603");
  check("1.0", "0.11835170935876249132", GMP_RNDU, 6, 41, 36,
        "0.1183517093595583");
  check53("67108865.0", "134217729.0", GMP_RNDN, "9.007199456067584e15");
  check("1.37399642157394197284e-01", "2.28877275604219221350e-01", GMP_RNDN,
        49, 15, 32, "0.0314472340833162888");
  check("4.03160720978664954828e-01", "5.854828e-1"
        /*"5.85483042917246621073e-01"*/, GMP_RNDZ,
        51, 22, 32, "0.2360436821472831");
  check("3.90798504668055102229e-14", "9.85394674650308388664e-04", GMP_RNDN,
        46, 22, 12, "0.385027296503914762e-16");
  check("4.58687081072827851358e-01", "2.20543551472118792844e-01", GMP_RNDN,
        49, 3, 2, "0.09375");
  check_max();
  check_min();

  check_regression ();
  test_generic (2, 500, 100);

  data_check ("data/mulpi", mpfr_mulpi, "mpfr_mulpi");

  tests_end_mpfr ();
  return 0;
}
