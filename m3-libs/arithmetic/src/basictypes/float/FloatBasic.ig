GENERIC INTERFACE FloatBasic(R,Rt);
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

CONST
  Equal   = Rt.Equal;
  Compare = Rt.Compare;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)

(* lent from RealType.i3 and LongrealType.i3 since a ExtendedType.i3 is missing *)
<*INLINE*> PROCEDURE Equal(x,y:T): BOOLEAN;  (* return "a = b".
The result is undefined if either "a" or "b" is an "NaN" (not a number) value. *)

<*INLINE*> PROCEDURE Compare(x,y:T): [-1..1];
(* Return "-1" if "a < b",
           "0" if "a = b",
       or "+1" if "a > b".
   The result is undefined if either "a" or "b" is an "NaN" (not a number) value. *)

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T;  (*return x/y*)
<*INLINE*> PROCEDURE Mod(x,y:T):T;  (*return x mod y*)
<*INLINE*> PROCEDURE Rec(x:T):T;    (*return 1/x*)

(*==========================*)
END FloatBasic.
