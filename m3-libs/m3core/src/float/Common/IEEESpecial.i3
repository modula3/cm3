(* Copyright (C) 1990, Digital Equipment Corporation.                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 25 12:32:48 PST 1994 by kalsow                   *)
(*      modified on Wed Dec  8 17:33:29 PST 1993 by heydon                   *)

INTERFACE IEEESpecial;

(* This interface defines variables for the IEEE floating-point values
   -infinity, +infinity, and NaN (not a number) for each of the three
   floating-point types. On non-IEEE implementations, these variables
   have arbitrary values. *)

VAR (* CONST after initialization *)
  RealNegInf, RealPosInf, RealNan: REAL;
  LongNegInf, LongPosInf, LongNan: LONGREAL;
  ExtdNegInf, ExtdPosInf, ExtdNan: EXTENDED;

END IEEESpecial.
