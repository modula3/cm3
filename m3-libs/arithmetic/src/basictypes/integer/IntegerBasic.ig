GENERIC INTERFACE IntegerBasic(I,It);
(*Copyright (c) 1996, m3na project

Abstract: Generic computations on integer types

2/17/96  Harry George    Initial version
*)

FROM NADefinitions IMPORT Error;

(*==========================*)

CONST
  Brand = It.Brand;

TYPE
  T = I.T;
  QuotRem = RECORD quot, rem: T END;
  Array = REF ARRAY OF T;

CONST
  Zero        =  0;
  One         =  1;
  Two         =  2;
  MinusOne    = -1;

CONST
  Equal   = It.Equal;
  Compare = It.Compare;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(x:T):T;   (*return x*)
<*INLINE*> PROCEDURE IsZero(x:T):BOOLEAN;

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(x:T):T RAISES {Error};  (*return 1/x*)
<*INLINE*> PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return x mod y*)
<*INLINE*> PROCEDURE DivMod(x,y:T): QuotRem RAISES {Error};  (*return x/y and write the remainder in r*)
<*OBSOLETE*>CONST ScaleInt = Mul;

PROCEDURE GCD(x,y:T):T;
(*greatest common divisor for machines with slow division*)

(*==========================*)
END IntegerBasic.
