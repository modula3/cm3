(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Complex numbers: pairs of arbitrary Scheme numbers.
   If imaginary part = 0, New() demotes to the real part. *)

INTERFACE SchemeComplex;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T = BRANDED "SchemeComplex" REF
           RECORD re, im: SchemeObject.T END;

(* Construction *)
PROCEDURE New(re, im: SchemeObject.T): SchemeObject.T;
  (* Demotes to real if im is exactly 0 *)
PROCEDURE MakeRectangular(re, im: SchemeObject.T):
    SchemeObject.T RAISES {E};
PROCEDURE MakePolar(mag, ang: SchemeObject.T):
    SchemeObject.T RAISES {E};

(* Accessors *)
PROCEDURE RealPart(x: T): SchemeObject.T;
PROCEDURE ImagPart(x: T): SchemeObject.T;
PROCEDURE Magnitude(x: SchemeObject.T): SchemeObject.T RAISES {E};
  (* Works on real numbers too: returns abs *)
PROCEDURE Angle(x: SchemeObject.T): SchemeObject.T RAISES {E};
  (* Works on real numbers too: returns 0 or pi *)

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

END SchemeComplex.
