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
PROCEDURE Mod(x,y:T):T = BEGIN RETURN x MOD y END Mod;
PROCEDURE Rec(x:T):T   = BEGIN RETURN One/x END Rec;

(*==========================*)
BEGIN
END FloatBasic.
