GENERIC INTERFACE PolarBasic(R, C);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Complex numbers in polar coordinates *)

FROM NADefinitions IMPORT Error;

CONST Brand = R.Brand & "Polar";

TYPE
  (*polar angles are in radians*)
  T = RECORD radius, angle: R.T;  END;

<*INLINE*>
PROCEDURE IsZero (READONLY x: T): BOOLEAN;
<*INLINE*>
PROCEDURE Equal (READONLY x, y: T): BOOLEAN; (*return x=y*)

PROCEDURE FromComplex (READONLY c: C.T): T;
PROCEDURE ToComplex (READONLY c: T): C.T;

PROCEDURE Mul (READONLY x, y: T): T; (*return x*y*)
PROCEDURE Div (READONLY x, y: T): T RAISES {Error}; (*return x/y*)

(*==========================*)
END PolarBasic.
