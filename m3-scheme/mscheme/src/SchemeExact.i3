(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Exact-integer abstraction.  Operates on SchemeObject.T values
   that are fixnums (REF INTEGER) or bignums (Mpz.T), without
   exposing which.  Phase 2 will extend to exact rationals. *)

INTERFACE SchemeExact;
IMPORT SchemeObject;

(* Predicate *)
PROCEDURE Is(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is an exact integer (fixnum or bignum) *)

PROCEDURE IsInteger(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is an exact integer.
     (Same as Is in Phase 1; differs from Is in Phase 2
      when exact rationals are added.) *)

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
