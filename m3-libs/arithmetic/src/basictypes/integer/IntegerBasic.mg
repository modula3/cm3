GENERIC MODULE IntegerBasic();
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

FROM NADefinitions IMPORT Error, Err;
IMPORT Word AS W;

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
    RETURN x DIV y;
  END Div;
PROCEDURE Rec(x:T):T RAISES {Error} =
  BEGIN
    CASE x OF
      | 0 => RAISE Error(Err.divide_by_zero);
      | 1 => RETURN 1;
    ELSE
      RAISE Error(Err.indivisible);
    END;
  END Rec;

PROCEDURE Mod(x,y:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    RETURN x MOD y;
  END Mod;
PROCEDURE DivMod(x,y:T;VAR (*OUT*) r:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    r:=x MOD y;
    RETURN x DIV y;
  END DivMod;


PROCEDURE GCD(x,y:T):T=
VAR
  xt,yt:[0..BITSIZE(T)]:=0;
  z:T:=One;
BEGIN
  IF x=0 THEN
    RETURN y;
  END;
  IF y=0 THEN
    RETURN x;
  END;
  (*This will be optimized to bit shift operations I hope*)
  (*count the factor 2*)
  WHILE x MOD 2 = 0 DO
    x:=x DIV 2;
    INC(xt);
  END;
  WHILE y MOD 2 = 0 DO
    y:=y DIV 2;
    INC(yt);
  END;
  WHILE x#y DO
    IF x<=y THEN
      z:=y-x;
    ELSE
      z:=x-y;
      x:=y;
    END;
    (*both x and y are odd, thus there difference is even*)
    WHILE z MOD 2 = 0 DO
      z:=z DIV 2;
    END;
    y:=z;
  END;
  RETURN W.LeftShift(x,MIN(xt,yt));
END GCD;

(*==========================*)
BEGIN
END IntegerBasic.
