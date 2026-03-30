(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Exact rational numbers: pairs of Mpz.T in lowest terms.
   Denominator is always positive.  If denominator = 1, New()
   demotes to an exact integer. *)

INTERFACE SchemeRational;
IMPORT SchemeObject, Mpz;

TYPE T = BRANDED "SchemeRational" REF RECORD num, den: Mpz.T END;

PROCEDURE New(num, den: Mpz.T): SchemeObject.T;
  (* Normalize to lowest terms, ensure den > 0, demote if den = 1.
     Checked runtime error if den = 0. *)

(* Arithmetic — operands must be SchemeRational.T *)
PROCEDURE Add(a, b: T): SchemeObject.T;
PROCEDURE Sub(a, b: T): SchemeObject.T;
PROCEDURE Mul(a, b: T): SchemeObject.T;
PROCEDURE Div(a, b: T): SchemeObject.T;
PROCEDURE Neg(a: T): SchemeObject.T;
PROCEDURE Abs(a: T): SchemeObject.T;

(* Comparison *)
PROCEDURE Compare(a, b: T): INTEGER;
  (* Returns -1, 0, or 1 *)

(* Sign predicates *)
PROCEDURE IsZero(a: T): BOOLEAN;
PROCEDURE IsPositive(a: T): BOOLEAN;
PROCEDURE IsNegative(a: T): BOOLEAN;

(* Accessors *)
PROCEDURE Numerator(x: T): SchemeObject.T;
  (* Returns exact integer numerator *)
PROCEDURE Denominator(x: T): SchemeObject.T;
  (* Returns exact integer denominator (always positive) *)

(* Conversion *)
PROCEDURE ToLongReal(x: T): LONGREAL;
  (* num/den as LONGREAL; may lose precision *)
PROCEDURE Format(x: T): TEXT;
  (* Returns "num/den" *)

(* Helpers for mixed integer/rational arithmetic *)
PROCEDURE FromInt(x: SchemeObject.T): T;
  (* Convert an exact integer (REF INTEGER or Mpz.T) to rational
     with denominator 1.  Does NOT normalize (den is already 1). *)

END SchemeRational.
