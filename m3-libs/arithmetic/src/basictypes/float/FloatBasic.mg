GENERIC MODULE FloatBasic();
(*Copyright (c) 1996, m3na project
  
Abstract: Generic wrapper routines for basic operations of float types

*)

<*UNUSED*> CONST Module = "FloatBasic.";
(*==========================*)

(*TYPE T = R.T;*)

PROCEDURE Add(x,y:T):T = BEGIN RETURN x+y END Add;
PROCEDURE Sub(x,y:T):T = BEGIN RETURN x-y END Sub;
PROCEDURE Neg(x:T):T   = BEGIN RETURN -x  END Neg;

PROCEDURE Mul(x,y:T):T = BEGIN RETURN x*y END Mul;
PROCEDURE Div(x,y:T):T = BEGIN RETURN x/y END Div;
PROCEDURE Rec(x:T):T   = BEGIN RETURN FLOAT(1.0,T)/x END Rec;

(*==========================*)
BEGIN
END FloatBasic.
