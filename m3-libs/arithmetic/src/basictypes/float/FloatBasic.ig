GENERIC INTERFACE FloatBasic(R);
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for basic operations of float types

*)

(*==========================*)

TYPE T = R.T;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T;  (*return x/y*)
<*INLINE*> PROCEDURE Mod(x,y:T):T;  (*return x mod y*)
<*INLINE*> PROCEDURE Rec(x:T):T;    (*return 1/x*)

(*==========================*)
END FloatBasic.
