GENERIC INTERFACE ComplexFast(R,CB);
(*Copyright (c) 1996, m3na project

Abstract: Generic complex number type
          Fast implementation using infix operators.
          Will become unnecessary if the compiler allows for INLINE procedures some day.
          Instantiate with Real, LongReal, Extended

Originally was xComplex

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations

*)

IMPORT FloatMode;
FROM NADefinitions IMPORT Error;

(*==========================*)

CONST
  Brand = CB.Brand;

TYPE
  T = CB.T;

CONST
  Zero     = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(0.0,R.T)};
  One      = T{re:=FLOAT( 1.0,R.T), im:=FLOAT(0.0,R.T)};
  I        = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(1.0,R.T)};
  MinusOne = T{re:=FLOAT(-1.0,R.T), im:=FLOAT(0.0,R.T)};

<*INLINE*> PROCEDURE Add(READONLY x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(READONLY x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(READONLY x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(READONLY x:T):T;   (*return complex conjugated of x*)
<*INLINE*> PROCEDURE IsZero(READONLY x:T):BOOLEAN;
<*INLINE*> PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

<*INLINE*> PROCEDURE Mul(READONLY x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(READONLY x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)
<*INLINE*> PROCEDURE Mod(READONLY x,y:T):T RAISES {Error};  (*return x mod y, which is always 0 for rational numbers*)
<*INLINE*> PROCEDURE DivMod(READONLY x,y:T;VAR r:T):T RAISES {Error};  (*return x/y and write the remainder in r, which is in turn 0*)

<*INLINE*> PROCEDURE Square(READONLY x:T):T;         (*return x*x*)
<*INLINE*> PROCEDURE Scale (READONLY x:T; y:R.T):T;  (*return x*y*)
<*OBSOLETE*> PROCEDURE ScaleInt(x:T;y:INTEGER):T;  (*return x*y*)

(*---- Floating point representations ----*)

<*INLINE*> PROCEDURE Normalize (READONLY x: T; VAR exp: INTEGER): T;

<*INLINE*> PROCEDURE Scalb(READONLY x: T; n: INTEGER): T RAISES {FloatMode.Trap};

<*INLINE*> PROCEDURE ILogb(READONLY x: T): INTEGER;
(*This is not really a logarithm of x or its absolute value,
  but something between the magnitudes of x.re and x.im,
  usable for normalization.*)

(*==========================*)
END ComplexFast.
