(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Full numeric tower dispatch.  Handles all combinations of
   exact and inexact operands, with exactness contagion. *)

INTERFACE SchemeNumber;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

(* Predicates *)
PROCEDURE Is(x: SchemeObject.T): BOOLEAN;
  (* TRUE for any number: exact or inexact *)

PROCEDURE IsExact(x: SchemeObject.T): BOOLEAN;

PROCEDURE IsInexact(x: SchemeObject.T): BOOLEAN;

(* Full-tower arithmetic *)
PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T RAISES {E};
PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T RAISES {E};
PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T RAISES {E};
PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T RAISES {E};
PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T RAISES {E};
PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T RAISES {E};

(* Full-tower comparison *)
PROCEDURE Compare(a, b: SchemeObject.T): INTEGER RAISES {E};
  (* Returns -1, 0, or 1 *)
PROCEDURE Equal(a, b: SchemeObject.T): BOOLEAN RAISES {E};
  (* Numeric equality *)

(* Real-only guard: raises if x is complex *)
PROCEDURE CheckReal(x: SchemeObject.T) RAISES {E};
  (* Raises "not a real number" for complex; no-op for reals *)

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN RAISES {E};
  (* TRUE for any numeric zero, including 0+0i *)

(* Conversion *)
PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL RAISES {E};
PROCEDURE ToInteger(x: SchemeObject.T): INTEGER RAISES {E};

(* Formatting *)
PROCEDURE Format(x: SchemeObject.T): TEXT;

END SchemeNumber.
