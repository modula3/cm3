GENERIC INTERFACE FloatBasic(R);
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for basic operations of float types

*)

(*==========================*)

TYPE T = R.T;

CONST
  Zero        = FLOAT(0.0D0,T);
  One         = FLOAT(1.0D0,T);
  Two         = FLOAT(2.0D0,T);
  MinusOne    = FLOAT(-1.0D0,T);

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T;  (*return x/y*)
<*INLINE*> PROCEDURE Mod(x,y:T):T;  (*return x mod y*)
<*INLINE*> PROCEDURE Rec(x:T):T;    (*return 1/x*)

(*==========================*)
END FloatBasic.
