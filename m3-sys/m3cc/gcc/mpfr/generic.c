/* generic file for evaluation of hypergeometric series using binary splitting

Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the MPFR Library.

The MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPdFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#ifndef GENERIC
# error You should specify a name
#endif

#ifdef B
#  ifndef A
#   error B cannot be used without A
#  endif
#endif

/* Compute the first 2^m terms from the hypergeometric series
   with x = p / 2^r */
static int
GENERIC (mpfr_ptr y, mpz_srcptr p, long r, int m)
{
  unsigned long n,i,k,j,l;
  int is_p_one;
  mpz_t* P,*S;
#ifdef A
  mpz_t *T;
#endif
  mpz_t* ptoj;
#ifdef R_IS_RATIONAL
  mpz_t* qtoj;
  mpfr_t tmp;
#endif
  mp_exp_t diff, expo;
  mp_prec_t precy = MPFR_PREC(y);
  MPFR_TMP_DECL(marker);

  MPFR_TMP_MARK(marker);
  MPFR_CLEAR_FLAGS(y);
  n = 1UL << m;
  P = (mpz_t*) MPFR_TMP_ALLOC ((m+1) * sizeof(mpz_t));
  S = (mpz_t*) MPFR_TMP_ALLOC ((m+1) * sizeof(mpz_t));
  ptoj = (mpz_t*) MPFR_TMP_ALLOC ((m+1) * sizeof(mpz_t)); /* ptoj[i] = mantissa^(2^i) */
#ifdef A
  T = (mpz_t*) MPFR_TMP_ALLOC ((m+1) * sizeof(mpz_t));
#endif
#ifdef R_IS_RATIONAL
  qtoj = (mpz_t*) MPFR_TMP_ALLOC ((m+1) * sizeof(mpz_t));
#endif
  for (i = 0 ; i <= m ; i++)
    {
      mpz_init (P[i]);
      mpz_init (S[i]);
      mpz_init (ptoj[i]);
#ifdef R_IS_RATIONAL
      mpz_init (qtoj[i]);
#endif
#ifdef A
      mpz_init (T[i]);
#endif
    }
  mpz_set (ptoj[0], p);
#ifdef C
#  if C2 != 1
  mpz_mul_ui (ptoj[0], ptoj[0], C2);
#  endif
#endif
  is_p_one = mpz_cmp_ui(ptoj[0], 1) == 0;
#ifdef A
#  ifdef B
  mpz_set_ui (T[0], A1 * B1);
#  else
  mpz_set_ui (T[0], A1);
#  endif
#endif
  if (!is_p_one)
    for (i = 1 ; i < m ; i++)
      mpz_mul (ptoj[i], ptoj[i-1], ptoj[i-1]);
#ifdef R_IS_RATIONAL
  mpz_set_si (qtoj[0], r);
  for (i = 1 ; i <= m ; i++)
    mpz_mul(qtoj[i], qtoj[i-1], qtoj[i-1]);
#endif
  mpz_set_ui (P[0], 1);
  mpz_set_ui (S[0], 1);

  k = 0;
  for (i = 1 ; i < n ; i++) {
    k++;

#ifdef A
#  ifdef B
    mpz_set_ui (T[k], (A1 + A2*i)*(B1+B2*i));
#  else
    mpz_set_ui (T[k], A1 + A2*i);
#  endif
#endif

#ifdef C
#  ifdef NO_FACTORIAL
    mpz_set_ui (P[k], (C1 + C2 * (i-1)));
    mpz_set_ui (S[k], 1);
#  else
    mpz_set_ui (P[k], (i+1) * (C1 + C2 * (i-1)));
    mpz_set_ui (S[k], i+1);
#  endif
#else
#  ifdef NO_FACTORIAL
    mpz_set_ui (P[k], 1);
#  else
    mpz_set_ui (P[k], i+1);
#  endif
    mpz_set (S[k], P[k]);
#endif

    for (j = i+1, l = 0 ; (j & 1) == 0 ; l++, j>>=1, k--) {
      if (!is_p_one)
        mpz_mul (S[k], S[k], ptoj[l]);
#ifdef A
#  ifdef B
#    if (A2*B2) != 1
      mpz_mul_ui (P[k], P[k], A2*B2);
#    endif
#  else
#    if A2 != 1
      mpz_mul_ui (P[k], P[k], A2);
#  endif
#endif
      mpz_mul (S[k], S[k], T[k-1]);
#endif
      mpz_mul (S[k-1], S[k-1], P[k]);
#ifdef R_IS_RATIONAL
      mpz_mul (S[k-1], S[k-1], qtoj[l]);
#else
      mpz_mul_2exp (S[k-1], S[k-1], r*(1<<l));
#endif
      mpz_add (S[k-1], S[k-1], S[k]);
      mpz_mul (P[k-1], P[k-1], P[k]);
#ifdef A
      mpz_mul (T[k-1], T[k-1], T[k]);
#endif
    }
  }

  diff = mpz_sizeinbase(S[0],2) - 2*precy;
  expo = diff;
  if (diff >= 0)
    mpz_div_2exp(S[0],S[0],diff);
  else
    mpz_mul_2exp(S[0],S[0],-diff);
  diff = mpz_sizeinbase(P[0],2) - precy;
  expo -= diff;
  if (diff >=0)
    mpz_div_2exp(P[0],P[0],diff);
  else
    mpz_mul_2exp(P[0],P[0],-diff);

  mpz_tdiv_q(S[0], S[0], P[0]);
  mpfr_set_z(y, S[0], GMP_RNDD);
  MPFR_SET_EXP (y, MPFR_GET_EXP (y) + expo);

#ifdef R_IS_RATIONAL
  /* exact division */
  mpz_div_ui (qtoj[m], qtoj[m], r);
  mpfr_init2 (tmp, MPFR_PREC(y));
  mpfr_set_z (tmp, qtoj[m] , GMP_RNDD);
  mpfr_div (y, y, tmp, GMP_RNDD);
  mpfr_clear (tmp);
#else
  mpfr_div_2ui(y, y, r*(i-1), GMP_RNDN);
#endif
  for (i = 0 ; i <= m ; i++)
    {
      mpz_clear (P[i]);
      mpz_clear (S[i]);
      mpz_clear (ptoj[i]);
#ifdef R_IS_RATIONAL
      mpz_clear (qtoj[i]);
#endif
#ifdef A
      mpz_clear (T[i]);
#endif
    }
  MPFR_TMP_FREE (marker);
  return 0;
}




