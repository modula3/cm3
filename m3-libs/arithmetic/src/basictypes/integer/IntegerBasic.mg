GENERIC MODULE IntegerBasic();
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "IntegerBasic.";
(*==========================*)
(*----------------------*)

PROCEDURE Add(x,y:T):T = BEGIN RETURN x+y END Add;
PROCEDURE Sub(x,y:T):T = BEGIN RETURN x-y END Sub;
PROCEDURE Neg(x:T):T   = BEGIN RETURN -x  END Neg;
PROCEDURE Conj(x:T):T  = BEGIN RETURN  x  END Conj;

PROCEDURE IsZero(x:T):BOOLEAN =
  BEGIN
    RETURN x=Zero;
  END IsZero;

PROCEDURE Mul(x,y:T):T = BEGIN RETURN x*y END Mul;
PROCEDURE Div(x,y:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    IF x MOD y # 0 THEN RAISE Error(Err.indivisible) END;
    RETURN x DIV y
  END Div;
PROCEDURE Mod(x,y:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    RETURN x MOD y;
  END Mod;
PROCEDURE DivMod(x,y:T;VAR r:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    r:=x MOD y;
    RETURN x DIV y;
  END DivMod;

(*==========================*)
BEGIN
END IntegerBasic.
