(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE MpzOps EXPORTS Mpz;
IMPORT MpzRep;
IMPORT MpzP AS P;
IMPORT Word;
IMPORT M3toC;

PROCEDURE abs (f0 : T; f1 : T) =
  BEGIN
    P.c_abs(ADR(f0.val),ADR(f1.val))
  END abs;

PROCEDURE add (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_add(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END add;

PROCEDURE add_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_add_ui(ADR(f0.val),ADR(f1.val),f2)
  END add_ui;

PROCEDURE addmul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_addmul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END addmul;

PROCEDURE addmul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_addmul_ui(ADR(f0.val),ADR(f1.val),f2)
  END addmul_ui;

PROCEDURE and (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_and(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END and;

PROCEDURE bin_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_bin_ui(ADR(f0.val),ADR(f1.val),f2)
  END bin_ui;

PROCEDURE bin_uiui (f0 : T; f1 : Word.T; f2 : Word.T) =
  BEGIN
    P.c_bin_uiui(ADR(f0.val),f1,f2)
  END bin_uiui;

PROCEDURE cdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_cdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END cdiv_q;

PROCEDURE cdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_cdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END cdiv_q_2exp;

PROCEDURE cdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_cdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END cdiv_q_ui;

PROCEDURE cdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_cdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END cdiv_qr;

PROCEDURE cdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_cdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END cdiv_qr_ui;

PROCEDURE cdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_cdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END cdiv_r;

PROCEDURE cdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_cdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END cdiv_r_2exp;

PROCEDURE cdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_cdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END cdiv_r_ui;

PROCEDURE cdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_cdiv_ui(ADR(f0.val),f1)
  END cdiv_ui;

PROCEDURE clear (f0 : T) =
  BEGIN
    P.c_clear(ADR(f0.val))
  END clear;

PROCEDURE clrbit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_clrbit(ADR(f0.val),f1)
  END clrbit;

PROCEDURE cmp (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_cmp(ADR(f0.val),ADR(f1.val))
  END cmp;

PROCEDURE cmp_d (f0 : T; f1 : LONGREAL) : INTEGER =
  BEGIN
    RETURN P.c_cmp_d(ADR(f0.val),f1)
  END cmp_d;

PROCEDURE cmpabs (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_cmpabs(ADR(f0.val),ADR(f1.val))
  END cmpabs;

PROCEDURE cmpabs_d (f0 : T; f1 : LONGREAL) : INTEGER =
  BEGIN
    RETURN P.c_cmpabs_d(ADR(f0.val),f1)
  END cmpabs_d;

PROCEDURE cmpabs_ui (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_cmpabs_ui(ADR(f0.val),f1)
  END cmpabs_ui;

PROCEDURE com (f0 : T; f1 : T) =
  BEGIN
    P.c_com(ADR(f0.val),ADR(f1.val))
  END com;

PROCEDURE combit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_combit(ADR(f0.val),f1)
  END combit;

PROCEDURE congruent_p (f0 : T; f1 : T; f2 : T) : INTEGER =
  BEGIN
    RETURN P.c_congruent_p(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END congruent_p;

PROCEDURE congruent_2exp_p (f0 : T; f1 : T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_congruent_2exp_p(ADR(f0.val),ADR(f1.val),f2)
  END congruent_2exp_p;

PROCEDURE congruent_ui_p (f0 : T; f1 : Word.T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_congruent_ui_p(ADR(f0.val),f1,f2)
  END congruent_ui_p;

PROCEDURE divexact (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_divexact(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END divexact;

PROCEDURE divexact_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_divexact_ui(ADR(f0.val),ADR(f1.val),f2)
  END divexact_ui;

PROCEDURE divisible_p (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_divisible_p(ADR(f0.val),ADR(f1.val))
  END divisible_p;

PROCEDURE divisible_ui_p (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_divisible_ui_p(ADR(f0.val),f1)
  END divisible_ui_p;

PROCEDURE divisible_2exp_p (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_divisible_2exp_p(ADR(f0.val),f1)
  END divisible_2exp_p;

PROCEDURE dump (f0 : T) =
  BEGIN
    P.c_dump(ADR(f0.val))
  END dump;

PROCEDURE fac_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_fac_ui(ADR(f0.val),f1)
  END fac_ui;

PROCEDURE fdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_fdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END fdiv_q;

PROCEDURE fdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_fdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END fdiv_q_2exp;

PROCEDURE fdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_fdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END fdiv_q_ui;

PROCEDURE fdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_fdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END fdiv_qr;

PROCEDURE fdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_fdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END fdiv_qr_ui;

PROCEDURE fdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_fdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END fdiv_r;

PROCEDURE fdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_fdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END fdiv_r_2exp;

PROCEDURE fdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_fdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END fdiv_r_ui;

PROCEDURE fdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_fdiv_ui(ADR(f0.val),f1)
  END fdiv_ui;

PROCEDURE fib_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_fib_ui(ADR(f0.val),f1)
  END fib_ui;

PROCEDURE fib2_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_fib2_ui(ADR(f0.val),ADR(f1.val),f2)
  END fib2_ui;

PROCEDURE fits_sint_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_sint_p(ADR(f0.val))
  END fits_sint_p;

PROCEDURE fits_slong_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_slong_p(ADR(f0.val))
  END fits_slong_p;

PROCEDURE fits_sshort_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_sshort_p(ADR(f0.val))
  END fits_sshort_p;

PROCEDURE fits_uint_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_uint_p(ADR(f0.val))
  END fits_uint_p;

PROCEDURE fits_ulong_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_ulong_p(ADR(f0.val))
  END fits_ulong_p;

PROCEDURE fits_ushort_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_fits_ushort_p(ADR(f0.val))
  END fits_ushort_p;

PROCEDURE gcd (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_gcd(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END gcd;

PROCEDURE gcd_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_gcd_ui(ADR(f0.val),ADR(f1.val),f2)
  END gcd_ui;

PROCEDURE gcdext (f0 : T; f1 : T; f2 : T; f3 : T; f4 : T) =
  BEGIN
    P.c_gcdext(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val),ADR(f4.val))
  END gcdext;

PROCEDURE get_d (f0 : T) : LONGREAL =
  BEGIN
    RETURN P.c_get_d(ADR(f0.val))
  END get_d;

PROCEDURE get_si (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_get_si(ADR(f0.val))
  END get_si;

PROCEDURE get_ui (f0 : T) : Word.T =
  BEGIN
    RETURN P.c_get_ui(ADR(f0.val))
  END get_ui;

PROCEDURE hamdist (f0 : T; f1 : T) : Word.T =
  BEGIN
    RETURN P.c_hamdist(ADR(f0.val),ADR(f1.val))
  END hamdist;

PROCEDURE init (f0 : T) =
  BEGIN
    P.c_init(ADR(f0.val))
  END init;

PROCEDURE init2 (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_init2(ADR(f0.val),f1)
  END init2;

PROCEDURE init_set (f0 : T; f1 : T) =
  BEGIN
    P.c_init_set(ADR(f0.val),ADR(f1.val))
  END init_set;

PROCEDURE init_set_d (f0 : T; f1 : LONGREAL) =
  BEGIN
    P.c_init_set_d(ADR(f0.val),f1)
  END init_set_d;

PROCEDURE init_set_si (f0 : T; f1 : INTEGER) =
  BEGIN
    P.c_init_set_si(ADR(f0.val),f1)
  END init_set_si;

PROCEDURE init_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_init_set_str(ADR(f0.val),M3toC.CopyTtoS(f1),f2)
  END init_set_str;

PROCEDURE init_set_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_init_set_ui(ADR(f0.val),f1)
  END init_set_ui;

PROCEDURE invert (f0 : T; f1 : T; f2 : T) : INTEGER =
  BEGIN
    RETURN P.c_invert(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END invert;

PROCEDURE ior (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_ior(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END ior;

PROCEDURE jacobi (f0 : T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_jacobi(ADR(f0.val),ADR(f1.val))
  END jacobi;

PROCEDURE kronecker_ui (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_kronecker_ui(ADR(f0.val),f1)
  END kronecker_ui;

PROCEDURE ui_kronecker (f0 : Word.T; f1 : T) : INTEGER =
  BEGIN
    RETURN P.c_ui_kronecker(f0,ADR(f1.val))
  END ui_kronecker;

PROCEDURE lcm (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_lcm(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END lcm;

PROCEDURE lcm_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_lcm_ui(ADR(f0.val),ADR(f1.val),f2)
  END lcm_ui;

PROCEDURE lucnum_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_lucnum_ui(ADR(f0.val),f1)
  END lucnum_ui;

PROCEDURE lucnum2_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_lucnum2_ui(ADR(f0.val),ADR(f1.val),f2)
  END lucnum2_ui;

PROCEDURE millerrabin (f0 : T; f1 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_millerrabin(ADR(f0.val),f1)
  END millerrabin;

PROCEDURE mod (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mod(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mod;

PROCEDURE mul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_mul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END mul;

PROCEDURE mul_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mul_2exp(ADR(f0.val),ADR(f1.val),f2)
  END mul_2exp;

PROCEDURE mul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_mul_ui(ADR(f0.val),ADR(f1.val),f2)
  END mul_ui;

PROCEDURE neg (f0 : T; f1 : T) =
  BEGIN
    P.c_neg(ADR(f0.val),ADR(f1.val))
  END neg;

PROCEDURE nextprime (f0 : T; f1 : T) =
  BEGIN
    P.c_nextprime(ADR(f0.val),ADR(f1.val))
  END nextprime;

PROCEDURE perfect_power_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_perfect_power_p(ADR(f0.val))
  END perfect_power_p;

PROCEDURE perfect_square_p (f0 : T) : INTEGER =
  BEGIN
    RETURN P.c_perfect_square_p(ADR(f0.val))
  END perfect_square_p;

PROCEDURE popcount (f0 : T) : Word.T =
  BEGIN
    RETURN P.c_popcount(ADR(f0.val))
  END popcount;

PROCEDURE pow_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_pow_ui(ADR(f0.val),ADR(f1.val),f2)
  END pow_ui;

PROCEDURE powm (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_powm(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END powm;

PROCEDURE powm_ui (f0 : T; f1 : T; f2 : Word.T; f3 : T) =
  BEGIN
    P.c_powm_ui(ADR(f0.val),ADR(f1.val),f2,ADR(f3.val))
  END powm_ui;

PROCEDURE probab_prime_p (f0 : T; f1 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_probab_prime_p(ADR(f0.val),f1)
  END probab_prime_p;

PROCEDURE realloc2 (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_realloc2(ADR(f0.val),f1)
  END realloc2;

PROCEDURE remove (f0 : T; f1 : T; f2 : T) : Word.T =
  BEGIN
    RETURN P.c_remove(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END remove;

PROCEDURE root (f0 : T; f1 : T; f2 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_root(ADR(f0.val),ADR(f1.val),f2)
  END root;

PROCEDURE rootrem (f0 : T; f1 : T; f2 : T; f3 : Word.T) =
  BEGIN
    P.c_rootrem(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END rootrem;

PROCEDURE scan0 (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_scan0(ADR(f0.val),f1)
  END scan0;

PROCEDURE scan1 (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_scan1(ADR(f0.val),f1)
  END scan1;

PROCEDURE set (f0 : T; f1 : T) =
  BEGIN
    P.c_set(ADR(f0.val),ADR(f1.val))
  END set;

PROCEDURE set_d (f0 : T; f1 : LONGREAL) =
  BEGIN
    P.c_set_d(ADR(f0.val),f1)
  END set_d;

PROCEDURE set_si (f0 : T; f1 : INTEGER) =
  BEGIN
    P.c_set_si(ADR(f0.val),f1)
  END set_si;

PROCEDURE set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER =
  BEGIN
    RETURN P.c_set_str(ADR(f0.val),M3toC.CopyTtoS(f1),f2)
  END set_str;

PROCEDURE set_ui (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_set_ui(ADR(f0.val),f1)
  END set_ui;

PROCEDURE setbit (f0 : T; f1 : Word.T) =
  BEGIN
    P.c_setbit(ADR(f0.val),f1)
  END setbit;

PROCEDURE size (f0 : T) : CARDINAL =
  BEGIN
    RETURN P.c_size(ADR(f0.val))
  END size;

PROCEDURE sizeinbase (f0 : T; f1 : INTEGER) : CARDINAL =
  BEGIN
    RETURN P.c_sizeinbase(ADR(f0.val),f1)
  END sizeinbase;

PROCEDURE sqrt (f0 : T; f1 : T) =
  BEGIN
    P.c_sqrt(ADR(f0.val),ADR(f1.val))
  END sqrt;

PROCEDURE sqrtrem (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_sqrtrem(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END sqrtrem;

PROCEDURE sub (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_sub(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END sub;

PROCEDURE sub_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_sub_ui(ADR(f0.val),ADR(f1.val),f2)
  END sub_ui;

PROCEDURE ui_sub (f0 : T; f1 : Word.T; f2 : T) =
  BEGIN
    P.c_ui_sub(ADR(f0.val),f1,ADR(f2.val))
  END ui_sub;

PROCEDURE submul (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_submul(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END submul;

PROCEDURE submul_ui (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_submul_ui(ADR(f0.val),ADR(f1.val),f2)
  END submul_ui;

PROCEDURE swap (f0 : T; f1 : T) =
  BEGIN
    P.c_swap(ADR(f0.val),ADR(f1.val))
  END swap;

PROCEDURE tdiv_ui (f0 : T; f1 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_tdiv_ui(ADR(f0.val),f1)
  END tdiv_ui;

PROCEDURE tdiv_q (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_tdiv_q(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END tdiv_q;

PROCEDURE tdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_tdiv_q_2exp(ADR(f0.val),ADR(f1.val),f2)
  END tdiv_q_2exp;

PROCEDURE tdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_tdiv_q_ui(ADR(f0.val),ADR(f1.val),f2)
  END tdiv_q_ui;

PROCEDURE tdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T) =
  BEGIN
    P.c_tdiv_qr(ADR(f0.val),ADR(f1.val),ADR(f2.val),ADR(f3.val))
  END tdiv_qr;

PROCEDURE tdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_tdiv_qr_ui(ADR(f0.val),ADR(f1.val),ADR(f2.val),f3)
  END tdiv_qr_ui;

PROCEDURE tdiv_r (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_tdiv_r(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END tdiv_r;

PROCEDURE tdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T) =
  BEGIN
    P.c_tdiv_r_2exp(ADR(f0.val),ADR(f1.val),f2)
  END tdiv_r_2exp;

PROCEDURE tdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T =
  BEGIN
    RETURN P.c_tdiv_r_ui(ADR(f0.val),ADR(f1.val),f2)
  END tdiv_r_ui;

PROCEDURE tstbit (f0 : T; f1 : Word.T) : INTEGER =
  BEGIN
    RETURN P.c_tstbit(ADR(f0.val),f1)
  END tstbit;

PROCEDURE ui_pow_ui (f0 : T; f1 : Word.T; f2 : Word.T) =
  BEGIN
    P.c_ui_pow_ui(ADR(f0.val),f1,f2)
  END ui_pow_ui;

PROCEDURE xor (f0 : T; f1 : T; f2 : T) =
  BEGIN
    P.c_xor(ADR(f0.val),ADR(f1.val),ADR(f2.val))
  END xor;

PROCEDURE mpf_set_default_prec (f0 : Word.T) =
  BEGIN
    P.c_mpf_set_default_prec(f0)
  END mpf_set_default_prec;

BEGIN END MpzOps.
