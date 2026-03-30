(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Exact-number abstraction.  Operates on SchemeObject.T values
   that are fixnums (REF INTEGER), bignums (Mpz.T), or exact
   rationals (SchemeRational.T). *)

INTERFACE SchemeExact;
IMPORT SchemeObject;

(* Predicate *)
PROCEDURE Is(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is exact: integer or rational *)

PROCEDURE IsInteger(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is an exact integer (fixnum or bignum).
     FALSE for rationals (rationals with den=1 are demoted). *)

(* Sign predicates — operands must be exact *)
PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN;
PROCEDURE IsPositive(x: SchemeObject.T): BOOLEAN;
PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN;

(* Arithmetic — operands must be exact integers *)
PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T;
  (* Truncated quotient *)
PROCEDURE Rem(a, b: SchemeObject.T): SchemeObject.T;
  (* Truncated remainder *)
PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T;
PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T;

(* Comparison — operands must be exact integers *)
PROCEDURE Compare(a, b: SchemeObject.T): INTEGER;
  (* Returns -1, 0, or 1 *)

(* Conversion *)
PROCEDURE ToInteger(x: SchemeObject.T): INTEGER;
  (* Checked runtime error if not exact or out of range *)
PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL;
  (* Exact integer -> LONGREAL (may lose precision for large values) *)
PROCEDURE Format(x: SchemeObject.T): TEXT;
  (* Decimal string representation *)

(* Construction *)
PROCEDURE FromI(x: INTEGER): SchemeObject.T;
  (* Convenience: delegates to SchemeInt.FromI *)

VAR (* CONST, set in module body *)
  Zero, One, NegOne, Two: SchemeObject.T;

END SchemeExact.
