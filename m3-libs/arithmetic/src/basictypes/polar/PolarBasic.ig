GENERIC INTERFACE PolarBasic(C,R);
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers in polar coordinates

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;

TYPE
   (*polar angles are in radians*)
   T  = RECORD radius,angle:R.T; END;

PROCEDURE FromComplex(READONLY c:C.T):T;
PROCEDURE ToComplex(READONLY c:T):C.T;

PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

PROCEDURE Mul(READONLY x,y:T):T;     (*return x*y*)
PROCEDURE Div(READONLY x,y:T):T RAISES {Error};     (*return x/y*)

(*==========================*)
END PolarBasic.
