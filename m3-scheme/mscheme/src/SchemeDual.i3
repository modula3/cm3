(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Dual numbers: a + b*epsilon where epsilon^2 = 0.
   Used for forward-mode automatic differentiation.
   If epsilon part = 0 (exact), New() demotes to the real part. *)

INTERFACE SchemeDual;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T = BRANDED "SchemeDual" REF
           RECORD re, eps: SchemeObject.T END;

(* Construction *)
PROCEDURE New(re, eps: SchemeObject.T): SchemeObject.T;
  (* Demotes to re if eps is exactly 0 *)

(* Accessors *)
PROCEDURE RealPart(x: T): SchemeObject.T;
PROCEDURE EpsilonPart(x: T): SchemeObject.T;

(* Arithmetic *)
PROCEDURE Add(a, b: T): SchemeObject.T RAISES {E};
PROCEDURE Sub(a, b: T): SchemeObject.T RAISES {E};
PROCEDURE Mul(a, b: T): SchemeObject.T RAISES {E};
PROCEDURE Div(a, b: T): SchemeObject.T RAISES {E};
PROCEDURE Neg(a: T): SchemeObject.T RAISES {E};

(* Equality *)
PROCEDURE Equal(a, b: T): BOOLEAN RAISES {E};

(* Formatting *)
PROCEDURE Format(x: T): TEXT;

END SchemeDual.
