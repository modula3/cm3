GENERIC MODULE FloatBasic();
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for basic operations of float types

*)

FROM NADefinitions IMPORT Error, Err;

<*UNUSED*> CONST Module = "FloatBasic.";
(*==========================*)

PROCEDURE Add(x,y:T):T = BEGIN RETURN x+y END Add;
PROCEDURE Sub(x,y:T):T = BEGIN RETURN x-y END Sub;
PROCEDURE Neg(x:T):T   = BEGIN RETURN -x  END Neg;
PROCEDURE Conj(x:T):T  = BEGIN RETURN  x  END Conj;

PROCEDURE IsZero(x:T):BOOLEAN =
  BEGIN
    RETURN x=Zero;
  END IsZero;

<*INLINE*>
PROCEDURE CheckDivisor(x:T) RAISES {Error} =
  BEGIN
    IF x=Zero THEN
      RAISE Error(Err.divide_by_zero);
    END;
  END CheckDivisor;

PROCEDURE Mul(x,y:T):T = BEGIN RETURN x*y END Mul;
PROCEDURE Div(x,y:T):T RAISES {Error} = BEGIN CheckDivisor(y); RETURN x/y END Div;
PROCEDURE Mod(<*UNUSED*>x:T;y:T):T RAISES {Error} = BEGIN CheckDivisor(y); RETURN Zero END Mod;
PROCEDURE DivMod(x,y:T;VAR (*OUT*) r:T):T RAISES {Error} = BEGIN CheckDivisor(y); r := Zero; RETURN x/y END DivMod;
PROCEDURE IntMod(x,y:T):T RAISES {Error} = BEGIN IF y=Zero THEN RAISE Error(Err.divide_by_zero) END; RETURN x MOD y END IntMod;
PROCEDURE Rec(x:T):T RAISES {Error}   = BEGIN IF x=Zero THEN RAISE Error(Err.divide_by_zero) END; RETURN One/x END Rec;
PROCEDURE ScaleInt(x:T;y:INTEGER):T = BEGIN RETURN x*FLOAT(y,T) END ScaleInt;

(*==========================*)
BEGIN
END FloatBasic.
