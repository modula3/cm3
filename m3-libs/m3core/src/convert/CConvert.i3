(* Copyright (C) 1990, 1992, Digital Equipment Corporation.     *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* Last modified on Tue Mar  3 18:39:13 PST 1992 by muller      *)
(*      modified on Thu Dec 21 11:40:01 1989 by kalsow          *)

(* low-level C routines for numeric conversions.

   Index: conversion;  numbers;  ASCII *)

INTERFACE CConvert;

FROM Ctypes IMPORT double, int, int_star, char_star, char_star_star; 

<* EXTERNAL m3_strtod *>
PROCEDURE strtod (nptr: ADDRESS; VAR eptr: ADDRESS): LONGREAL;
(* Returns a nearest machine number to the input decimal string
   (or sets errno to ERANGE).  With IEEE arithmetic, ties are broken
   by the IEEE round-even rule.  Otherwise ties are broken by biased
   rounding (add half and chop).  *)

<* EXTERNAL m3_dtoa*>
PROCEDURE dtoa (d: double;  mode: int;  ndigits: int;  decpt: int_star;
                sign: int_star;  rve: char_star_star): char_star;
(* Converts a C double to an ASCII string. *)

(* Arguments ndigits, decpt, sign are similar to those
   of ecvt and fcvt; trailing zeros are suppressed from
   the returned string.  If not null, *rve is set to point
   to the end of the return value.  If d is +-Infinity or NaN,
   then *decpt is set to 9999.

   mode:
     0 ==> shortest string that yields d when read in
           and rounded to nearest.
     1 ==> like 0, but with Steele & White stopping rule;
           e.g. with IEEE P754 arithmetic , mode 0 gives
           1e23 whereas mode 1 gives 9.999999999999999e22.
     2 ==> ndigits significant digits.  This gives a return
           value similar to that of ecvt, except that
           trailing zeros are suppressed.
     3 ==> through ndigits past the decimal point.  This
           gives a return value similar to that from fcvt,
           except that trailing zeros are suppressed.
     4-9 should give the same return values as 2-3, i.e.,
           4 <= mode <= 9 ==> same return as mode
           2 + (mode & 1).  These modes are mainly for
           debugging; often they run slower but sometimes
           faster than modes 2-3.
     4,5,8,9 ==> left-to-right digit generation.
     6-9 ==> don't try fast floating-point estimate
           (if applicable).

     Values of mode other than 0-9 are treated as mode 0.

     Sufficient space is allocated to the return value
     to hold the suppressed trailing zeros.
*)

END CConvert.

(* The implementations of these routines can be found in
   "m3core/src/Csupport/Common/dtoa.h".  They are derived from
   David M. Gay's code distributed by AT&T.
*)

