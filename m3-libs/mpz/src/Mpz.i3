(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(**** AUTOMATICALLY GENERATED -- DO NOT EDIT ****)

INTERFACE Mpz;

IMPORT Word;

TYPE
  T <: REFANY;

PROCEDURE New() : T;

PROCEDURE NewInt(int : INTEGER) : T;

PROCEDURE NewWord(w : Word.T) : T;

CONST Brand = "Mpz";


TYPE FormatBase = { Binary, Octal, Decimal, Hexadecimal };
     
PROCEDURE Format(t : T; base := FormatBase.Decimal) : TEXT;
  
PROCEDURE FormatDecimal(t : T) : TEXT;

PROCEDURE FormatHexadecimal(t : T) : TEXT;

PROCEDURE FormatOctal(t : T) : TEXT;

PROCEDURE FormatBased(t : T; base : [-2..62]) : TEXT;

PROCEDURE Import(t : T; READONLY data : ARRAY OF Word.T);

PROCEDURE Export(VAR data : ARRAY OF Word.T; t : T);

PROCEDURE InitScan(txt : TEXT; base : CARDINAL) : T;

PROCEDURE ToInteger(t : T) : INTEGER;
(* checked runtime error to be out of range *)

PROCEDURE ToWord(t : T) : Word.T;
(* checked runtime error to be out of range *)

PROCEDURE pow(p, b, x : T);

PROCEDURE LeftShift(f0 : T; f1 : T; amt : CARDINAL);

PROCEDURE RightShift(f0 : T; f1 : T; amt : CARDINAL);

PROCEDURE Shift(f0 : T; f1 : T; amt : INTEGER);
  (* negative -> right shift; positive -> left shift *)

PROCEDURE ShiftMpz(f0 : T; f1 : T; amt : T);
  (* negative -> right shift; positive -> left shift *)

PROCEDURE ShiftNegMpz(f0 : T; f1 : T; amt : T);
  (* negative -> left shift; positive -> right shift *)

(***** auto-generated functions *****)

PROCEDURE abs (f0 : T; f1 : T);

PROCEDURE add (f0 : T; f1 : T; f2 : T);

PROCEDURE add_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE addmul (f0 : T; f1 : T; f2 : T);

PROCEDURE addmul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE and (f0 : T; f1 : T; f2 : T);

PROCEDURE bin_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE bin_uiui (f0 : T; f1 : Word.T; f2 : Word.T);

PROCEDURE cdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE cdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE cdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE cdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE cdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE cdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE cdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE cdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE cdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE clear (f0 : T);

PROCEDURE clrbit (f0 : T; f1 : Word.T);

PROCEDURE cmp (f0 : T; f1 : T) : INTEGER;

PROCEDURE cmp_d (f0 : T; f1 : LONGREAL) : INTEGER;

PROCEDURE cmpabs (f0 : T; f1 : T) : INTEGER;

PROCEDURE cmpabs_d (f0 : T; f1 : LONGREAL) : INTEGER;

PROCEDURE cmpabs_ui (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE com (f0 : T; f1 : T);

PROCEDURE combit (f0 : T; f1 : Word.T);

PROCEDURE congruent_p (f0 : T; f1 : T; f2 : T) : INTEGER;

PROCEDURE congruent_2exp_p (f0 : T; f1 : T; f2 : Word.T) : INTEGER;

PROCEDURE congruent_ui_p (f0 : T; f1 : Word.T; f2 : Word.T) : INTEGER;

PROCEDURE divexact (f0 : T; f1 : T; f2 : T);

PROCEDURE divexact_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE divisible_p (f0 : T; f1 : T) : INTEGER;

PROCEDURE divisible_ui_p (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE divisible_2exp_p (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE dump (f0 : T);

PROCEDURE fac_ui (f0 : T; f1 : Word.T);

PROCEDURE fdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE fdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE fdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE fdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE fdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE fdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE fdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE fdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE fdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE fib_ui (f0 : T; f1 : Word.T);

PROCEDURE fib2_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE fits_sint_p (f0 : T) : INTEGER;

PROCEDURE fits_slong_p (f0 : T) : INTEGER;

PROCEDURE fits_sshort_p (f0 : T) : INTEGER;

PROCEDURE fits_uint_p (f0 : T) : INTEGER;

PROCEDURE fits_ulong_p (f0 : T) : INTEGER;

PROCEDURE fits_ushort_p (f0 : T) : INTEGER;

PROCEDURE gcd (f0 : T; f1 : T; f2 : T);

PROCEDURE gcd_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE gcdext (f0 : T; f1 : T; f2 : T; f3 : T; f4 : T);

PROCEDURE get_d (f0 : T) : LONGREAL;

PROCEDURE get_si (f0 : T) : INTEGER;

PROCEDURE get_ui (f0 : T) : Word.T;

PROCEDURE hamdist (f0 : T; f1 : T) : Word.T;

PROCEDURE init (f0 : T);

PROCEDURE init2 (f0 : T; f1 : Word.T);

PROCEDURE init_set (f0 : T; f1 : T);

PROCEDURE init_set_d (f0 : T; f1 : LONGREAL);

PROCEDURE init_set_si (f0 : T; f1 : INTEGER);

PROCEDURE init_set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER;

PROCEDURE init_set_ui (f0 : T; f1 : Word.T);

PROCEDURE invert (f0 : T; f1 : T; f2 : T) : INTEGER;

PROCEDURE ior (f0 : T; f1 : T; f2 : T);

PROCEDURE jacobi (f0 : T; f1 : T) : INTEGER;

PROCEDURE kronecker_ui (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE ui_kronecker (f0 : Word.T; f1 : T) : INTEGER;

PROCEDURE lcm (f0 : T; f1 : T; f2 : T);

PROCEDURE lcm_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE lucnum_ui (f0 : T; f1 : Word.T);

PROCEDURE lucnum2_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE millerrabin (f0 : T; f1 : INTEGER) : INTEGER;

PROCEDURE mod (f0 : T; f1 : T; f2 : T);

PROCEDURE mul (f0 : T; f1 : T; f2 : T);

PROCEDURE mul_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE mul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE neg (f0 : T; f1 : T);

PROCEDURE nextprime (f0 : T; f1 : T);

PROCEDURE perfect_power_p (f0 : T) : INTEGER;

PROCEDURE perfect_square_p (f0 : T) : INTEGER;

PROCEDURE popcount (f0 : T) : Word.T;

PROCEDURE pow_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE powm (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE powm_ui (f0 : T; f1 : T; f2 : Word.T; f3 : T);

PROCEDURE probab_prime_p (f0 : T; f1 : INTEGER) : INTEGER;

PROCEDURE realloc2 (f0 : T; f1 : Word.T);

PROCEDURE remove (f0 : T; f1 : T; f2 : T) : Word.T;

PROCEDURE root (f0 : T; f1 : T; f2 : Word.T) : INTEGER;

PROCEDURE rootrem (f0 : T; f1 : T; f2 : T; f3 : Word.T);

PROCEDURE scan0 (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE scan1 (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE set (f0 : T; f1 : T);

PROCEDURE set_d (f0 : T; f1 : LONGREAL);

PROCEDURE set_si (f0 : T; f1 : INTEGER);

PROCEDURE set_str (f0 : T; f1 : TEXT; f2 : INTEGER) : INTEGER;

PROCEDURE set_ui (f0 : T; f1 : Word.T);

PROCEDURE setbit (f0 : T; f1 : Word.T);

PROCEDURE size (f0 : T) : CARDINAL;

PROCEDURE sizeinbase (f0 : T; f1 : INTEGER) : CARDINAL;

PROCEDURE sqrt (f0 : T; f1 : T);

PROCEDURE sqrtrem (f0 : T; f1 : T; f2 : T);

PROCEDURE sub (f0 : T; f1 : T; f2 : T);

PROCEDURE sub_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE ui_sub (f0 : T; f1 : Word.T; f2 : T);

PROCEDURE submul (f0 : T; f1 : T; f2 : T);

PROCEDURE submul_ui (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE swap (f0 : T; f1 : T);

PROCEDURE tdiv_ui (f0 : T; f1 : Word.T) : Word.T;

PROCEDURE tdiv_q (f0 : T; f1 : T; f2 : T);

PROCEDURE tdiv_q_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE tdiv_q_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE tdiv_qr (f0 : T; f1 : T; f2 : T; f3 : T);

PROCEDURE tdiv_qr_ui (f0 : T; f1 : T; f2 : T; f3 : Word.T) : Word.T;

PROCEDURE tdiv_r (f0 : T; f1 : T; f2 : T);

PROCEDURE tdiv_r_2exp (f0 : T; f1 : T; f2 : Word.T);

PROCEDURE tdiv_r_ui (f0 : T; f1 : T; f2 : Word.T) : Word.T;

PROCEDURE tstbit (f0 : T; f1 : Word.T) : INTEGER;

PROCEDURE ui_pow_ui (f0 : T; f1 : Word.T; f2 : Word.T);

PROCEDURE xor (f0 : T; f1 : T; f2 : T);

PROCEDURE mpf_set_default_prec (f0 : Word.T);

END Mpz.
