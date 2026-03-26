(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MpzP;
IMPORT Word;
IMPORT Ctypes;
IMPORT Cstddef;

TYPE T       = MpzPtrT;
     MpzPtrT = ADDRESS;

(***** hand-coded functions *****)
<*EXTERNAL mpz_format_octal*>
PROCEDURE format_octal(z : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_format_decimal*>
PROCEDURE format_decimal(z : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_format_hexadecimal*>
PROCEDURE format_hexadecimal(z : MpzPtrT) : Ctypes.const_char_star;

<*EXTERNAL mpz_format_based*>
PROCEDURE format_based(z : MpzPtrT; base : Ctypes.int) : Ctypes.const_char_star;

<*EXTERNAL mpz_free_formatted*>
PROCEDURE free_formatted(f0 : Ctypes.char_star);

<*EXTERNAL "__gmpz_import"*>
PROCEDURE import(rop : MpzPtrT; count : Cstddef.size_t; order : Ctypes.int; size : Cstddef.size_t; endian : Ctypes.int; nails : Cstddef.size_t; op : ADDRESS);

<*EXTERNAL "__gmpz_export"*>
PROCEDURE export(rop : ADDRESS; count : ADDRESS; order : Ctypes.int; size : Cstddef.size_t; endian : Ctypes.int; nails : Cstddef.size_t; op : MpzPtrT);


(***** auto-generated functions *****)

<*EXTERNAL "__gmpz_abs" *>
PROCEDURE c_abs (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_add" *>
PROCEDURE c_add (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_add_ui" *>
PROCEDURE c_add_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_addmul" *>
PROCEDURE c_addmul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_addmul_ui" *>
PROCEDURE c_addmul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_and" *>
PROCEDURE c_and (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_bin_ui" *>
PROCEDURE c_bin_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_bin_uiui" *>
PROCEDURE c_bin_uiui (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_q" *>
PROCEDURE c_cdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_q_2exp" *>
PROCEDURE c_cdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_q_ui" *>
PROCEDURE c_cdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_qr" *>
PROCEDURE c_cdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_qr_ui" *>
PROCEDURE c_cdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_r" *>
PROCEDURE c_cdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_cdiv_r_2exp" *>
PROCEDURE c_cdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_cdiv_r_ui" *>
PROCEDURE c_cdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_cdiv_ui" *>
PROCEDURE c_cdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_clear" *>
PROCEDURE c_clear (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_clrbit" *>
PROCEDURE c_clrbit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_cmp" *>
PROCEDURE c_cmp (f0 : MpzPtrT; f1 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_cmp_d" *>
PROCEDURE c_cmp_d (f0 : MpzPtrT; f1 : LONGREAL) : Ctypes.int;

<*EXTERNAL "__gmpz_cmpabs" *>
PROCEDURE c_cmpabs (f0 : MpzPtrT; f1 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_cmpabs_d" *>
PROCEDURE c_cmpabs_d (f0 : MpzPtrT; f1 : LONGREAL) : Ctypes.int;

<*EXTERNAL "__gmpz_cmpabs_ui" *>
PROCEDURE c_cmpabs_ui (f0 : MpzPtrT; f1 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_com" *>
PROCEDURE c_com (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_combit" *>
PROCEDURE c_combit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_congruent_p" *>
PROCEDURE c_congruent_p (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_congruent_2exp_p" *>
PROCEDURE c_congruent_2exp_p (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_congruent_ui_p" *>
PROCEDURE c_congruent_ui_p (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_divexact" *>
PROCEDURE c_divexact (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_divexact_ui" *>
PROCEDURE c_divexact_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_divisible_p" *>
PROCEDURE c_divisible_p (f0 : MpzPtrT; f1 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_divisible_ui_p" *>
PROCEDURE c_divisible_ui_p (f0 : MpzPtrT; f1 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_divisible_2exp_p" *>
PROCEDURE c_divisible_2exp_p (f0 : MpzPtrT; f1 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_dump" *>
PROCEDURE c_dump (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_fac_ui" *>
PROCEDURE c_fac_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_fdiv_q" *>
PROCEDURE c_fdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_q_2exp" *>
PROCEDURE c_fdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fdiv_q_ui" *>
PROCEDURE c_fdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_qr" *>
PROCEDURE c_fdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_qr_ui" *>
PROCEDURE c_fdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_r" *>
PROCEDURE c_fdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_fdiv_r_2exp" *>
PROCEDURE c_fdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fdiv_r_ui" *>
PROCEDURE c_fdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fdiv_ui" *>
PROCEDURE c_fdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_fib_ui" *>
PROCEDURE c_fib_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_fib2_ui" *>
PROCEDURE c_fib2_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_fits_sint_p" *>
PROCEDURE c_fits_sint_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_fits_slong_p" *>
PROCEDURE c_fits_slong_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_fits_sshort_p" *>
PROCEDURE c_fits_sshort_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_fits_uint_p" *>
PROCEDURE c_fits_uint_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_fits_ulong_p" *>
PROCEDURE c_fits_ulong_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_fits_ushort_p" *>
PROCEDURE c_fits_ushort_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_gcd" *>
PROCEDURE c_gcd (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_gcd_ui" *>
PROCEDURE c_gcd_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_gcdext" *>
PROCEDURE c_gcdext (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT; f4 : MpzPtrT);

<*EXTERNAL "__gmpz_get_d" *>
PROCEDURE c_get_d (f0 : MpzPtrT) : LONGREAL;

<*EXTERNAL "__gmpz_get_si" *>
PROCEDURE c_get_si (f0 : MpzPtrT) : INTEGER;

<*EXTERNAL "__gmpz_get_ui" *>
PROCEDURE c_get_ui (f0 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_hamdist" *>
PROCEDURE c_hamdist (f0 : MpzPtrT; f1 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_init" *>
PROCEDURE c_init (f0 : MpzPtrT);

<*EXTERNAL "__gmpz_init2" *>
PROCEDURE c_init2 (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_init_set" *>
PROCEDURE c_init_set (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_init_set_d" *>
PROCEDURE c_init_set_d (f0 : MpzPtrT; f1 : LONGREAL);

<*EXTERNAL "__gmpz_init_set_si" *>
PROCEDURE c_init_set_si (f0 : MpzPtrT; f1 : INTEGER);

<*EXTERNAL "__gmpz_init_set_str" *>
PROCEDURE c_init_set_str (f0 : MpzPtrT; f1 : Ctypes.const_char_star; f2 : Ctypes.int) : Ctypes.int;

<*EXTERNAL "__gmpz_init_set_ui" *>
PROCEDURE c_init_set_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_invert" *>
PROCEDURE c_invert (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_ior" *>
PROCEDURE c_ior (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_jacobi" *>
PROCEDURE c_jacobi (f0 : MpzPtrT; f1 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_kronecker_ui" *>
PROCEDURE c_kronecker_ui (f0 : MpzPtrT; f1 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_ui_kronecker" *>
PROCEDURE c_ui_kronecker (f0 : Word.T; f1 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_lcm" *>
PROCEDURE c_lcm (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_lcm_ui" *>
PROCEDURE c_lcm_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_lucnum_ui" *>
PROCEDURE c_lucnum_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_lucnum2_ui" *>
PROCEDURE c_lucnum2_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_millerrabin" *>
PROCEDURE c_millerrabin (f0 : MpzPtrT; f1 : Ctypes.int) : Ctypes.int;

<*EXTERNAL "__gmpz_mod" *>
PROCEDURE c_mod (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_mul" *>
PROCEDURE c_mul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_mul_2exp" *>
PROCEDURE c_mul_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_mul_ui" *>
PROCEDURE c_mul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_neg" *>
PROCEDURE c_neg (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_nextprime" *>
PROCEDURE c_nextprime (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_perfect_power_p" *>
PROCEDURE c_perfect_power_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_perfect_square_p" *>
PROCEDURE c_perfect_square_p (f0 : MpzPtrT) : Ctypes.int;

<*EXTERNAL "__gmpz_popcount" *>
PROCEDURE c_popcount (f0 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_pow_ui" *>
PROCEDURE c_pow_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_powm" *>
PROCEDURE c_powm (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_powm_ui" *>
PROCEDURE c_powm_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_probab_prime_p" *>
PROCEDURE c_probab_prime_p (f0 : MpzPtrT; f1 : Ctypes.int) : Ctypes.int;

<*EXTERNAL "__gmpz_realloc2" *>
PROCEDURE c_realloc2 (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_remove" *>
PROCEDURE c_remove (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT) : Word.T;

<*EXTERNAL "__gmpz_root" *>
PROCEDURE c_root (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_rootrem" *>
PROCEDURE c_rootrem (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T);

<*EXTERNAL "__gmpz_scan0" *>
PROCEDURE c_scan0 (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_scan1" *>
PROCEDURE c_scan1 (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_set" *>
PROCEDURE c_set (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_set_d" *>
PROCEDURE c_set_d (f0 : MpzPtrT; f1 : LONGREAL);

<*EXTERNAL "__gmpz_set_si" *>
PROCEDURE c_set_si (f0 : MpzPtrT; f1 : INTEGER);

<*EXTERNAL "__gmpz_set_str" *>
PROCEDURE c_set_str (f0 : MpzPtrT; f1 : Ctypes.const_char_star; f2 : Ctypes.int) : Ctypes.int;

<*EXTERNAL "__gmpz_set_ui" *>
PROCEDURE c_set_ui (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_setbit" *>
PROCEDURE c_setbit (f0 : MpzPtrT; f1 : Word.T);

<*EXTERNAL "__gmpz_size" *>
PROCEDURE c_size (f0 : MpzPtrT) : CARDINAL;

<*EXTERNAL "__gmpz_sizeinbase" *>
PROCEDURE c_sizeinbase (f0 : MpzPtrT; f1 : Ctypes.int) : CARDINAL;

<*EXTERNAL "__gmpz_sqrt" *>
PROCEDURE c_sqrt (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_sqrtrem" *>
PROCEDURE c_sqrtrem (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_sub" *>
PROCEDURE c_sub (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_sub_ui" *>
PROCEDURE c_sub_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_ui_sub" *>
PROCEDURE c_ui_sub (f0 : MpzPtrT; f1 : Word.T; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_submul" *>
PROCEDURE c_submul (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_submul_ui" *>
PROCEDURE c_submul_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_swap" *>
PROCEDURE c_swap (f0 : MpzPtrT; f1 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_ui" *>
PROCEDURE c_tdiv_ui (f0 : MpzPtrT; f1 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_q" *>
PROCEDURE c_tdiv_q (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_q_2exp" *>
PROCEDURE c_tdiv_q_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_tdiv_q_ui" *>
PROCEDURE c_tdiv_q_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_qr" *>
PROCEDURE c_tdiv_qr (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_qr_ui" *>
PROCEDURE c_tdiv_qr_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT; f3 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tdiv_r" *>
PROCEDURE c_tdiv_r (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpz_tdiv_r_2exp" *>
PROCEDURE c_tdiv_r_2exp (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T);

<*EXTERNAL "__gmpz_tdiv_r_ui" *>
PROCEDURE c_tdiv_r_ui (f0 : MpzPtrT; f1 : MpzPtrT; f2 : Word.T) : Word.T;

<*EXTERNAL "__gmpz_tstbit" *>
PROCEDURE c_tstbit (f0 : MpzPtrT; f1 : Word.T) : Ctypes.int;

<*EXTERNAL "__gmpz_ui_pow_ui" *>
PROCEDURE c_ui_pow_ui (f0 : MpzPtrT; f1 : Word.T; f2 : Word.T);

<*EXTERNAL "__gmpz_xor" *>
PROCEDURE c_xor (f0 : MpzPtrT; f1 : MpzPtrT; f2 : MpzPtrT);

<*EXTERNAL "__gmpf_set_default_prec" *>
PROCEDURE c_mpf_set_default_prec (f0 : Word.T);

END MpzP.
