GENERIC INTERFACE FloatBasic(R,Rt);
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for basic operations of float types

*)

FROM xUtils IMPORT Error;

(*==========================*)

TYPE T = R.T;

CONST
  Zero        = FLOAT(0.0D0,T);
  One         = FLOAT(1.0D0,T);
  Two         = FLOAT(2.0D0,T);
  MinusOne    = FLOAT(-1.0D0,T);

CONST
  Equal   = Rt.Equal;
  Compare = Rt.Compare;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(x:T):T;   (*return x*)
<*INLINE*> PROCEDURE IsZero(x:T):BOOLEAN;

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return 0 normally, because there is no restriction for division for rational numbers*)
<*INLINE*> PROCEDURE DivMod(x,y:T;VAR r:T):T RAISES {Error};  (*return x/y and write the remainder 0 in r*)
<*INLINE*> PROCEDURE IntMod(x,y:T):T RAISES {Error};  (*return x mod y*)
<*INLINE*> PROCEDURE Rec(x:T):T RAISES {Error};    (*return 1/x*)
<*OBSOLETE*> PROCEDURE ScaleInt(x:T;y:INTEGER):T;  (*return x*y*)

(*==========================*)
END FloatBasic.
