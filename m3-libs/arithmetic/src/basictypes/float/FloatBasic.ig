GENERIC INTERFACE FloatBasic(R,RTy,RFl);
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for basic operations of float types

*)

FROM NADefinitions IMPORT Error;

(*==========================*)

CONST
  Brand = RTy.Brand;

TYPE
  T = R.T;
  Array = REF ARRAY OF T;

CONST
  Zero        = FLOAT(0.0D0,T);
  One         = FLOAT(1.0D0,T);
  Two         = FLOAT(2.0D0,T);
  MinusOne    = FLOAT(-1.0D0,T);

CONST
  Equal   = RTy.Equal;
  Compare = RTy.Compare;

<*INLINE*> PROCEDURE Add(x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(x:T):T;   (*return x*)
<*INLINE*> PROCEDURE IsZero(x:T):BOOLEAN;

<*INLINE*> PROCEDURE Mul(x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(x:T):T RAISES {Error};    (*return 1/x*)
<*INLINE*> PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return 0 normally, because there is no restriction for division for rational numbers*)
<*INLINE*> PROCEDURE DivMod(x,y:T;VAR r:T):T RAISES {Error};  (*return x/y and write the remainder 0 in r*)
<*INLINE*> PROCEDURE IntMod(x,y:T):T RAISES {Error};  (*return x mod y*)
CONST Scale = Mul;
<*OBSOLETE*> PROCEDURE ScaleInt(x:T;y:INTEGER):T;  (*return x*y*)

(*---- Floating point representations ----*)

CONST
  Scalb = RFl.Scalb;
  ILogb = RFl.ILogb;

(*==========================*)
END FloatBasic.
