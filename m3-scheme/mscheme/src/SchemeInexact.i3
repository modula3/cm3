(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Inexact-number abstraction.  Currently wraps SchemeLongReal.T;
   extensible to SchemeMpfr.T in future. *)

INTERFACE SchemeInexact;
IMPORT SchemeObject;

(* Predicate *)
PROCEDURE Is(x: SchemeObject.T): BOOLEAN;
  (* TRUE iff x is an inexact number *)

(* Arithmetic — operands must be inexact *)
PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T;
PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T;
PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T;

(* Comparison — operands must be inexact *)
PROCEDURE Compare(a, b: SchemeObject.T): INTEGER;
  (* Returns -1, 0, or 1 *)

(* Conversion *)
PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL;
PROCEDURE FromLongReal(x: LONGREAL): SchemeObject.T;
PROCEDURE FromInteger(x: INTEGER): SchemeObject.T;
PROCEDURE Format(x: SchemeObject.T): TEXT;
  (* Ensures decimal point in output *)

END SchemeInexact.
