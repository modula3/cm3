GENERIC INTERFACE ComplexFast(R);
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

FROM xUtils IMPORT Error;

(*==========================*)

TYPE
  T = RECORD re,im: R.T; END;

CONST
  Zero     = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(0.0,R.T)};
  One      = T{re:=FLOAT( 1.0,R.T), im:=FLOAT(0.0,R.T)};
  I        = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(1.0,R.T)};
  MinusOne = T{re:=FLOAT(-1.0,R.T), im:=FLOAT(0.0,R.T)};

<*INLINE*> PROCEDURE Add(READONLY x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(READONLY x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(READONLY x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(READONLY x:T):T;   (*return complex conjugated of x*)
<*INLINE*> PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

<*INLINE*> PROCEDURE Mul(READONLY x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(READONLY x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)

<*INLINE*> PROCEDURE Square(READONLY x:T):T;         (*return x*x*)
<*INLINE*> PROCEDURE Scale (READONLY x:T; y:R.T):T;  (*return x*y*)
<*INLINE*> PROCEDURE ScaleInt(x:T;y:INTEGER):T;  (*return x*y*)

(*==========================*)
END ComplexFast.
