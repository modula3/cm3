GENERIC MODULE ComplexBasic(R);
(*Copyright (c) 1996, m3na project
  
Abstract: Complex numbers and basic operations

was xComplex.m3

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)

FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "ComplexBasic.";
(*==========================*)

(*--------------*)
PROCEDURE Add(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=R.Add(x.re,y.re);
  z.im:=R.Add(x.im,y.im);
  RETURN z;
END Add;
(*--------------*)
PROCEDURE Sub(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=R.Sub(x.re,y.re);
  z.im:=R.Sub(x.im,y.im);
  RETURN z;
END Sub;

(*-------------------*)
PROCEDURE Neg(READONLY x : T) : T =
  VAR
    z : T;
  BEGIN
    z.re := R.Neg(x.re);
    z.im := R.Neg(x.im);
    RETURN z;
  END Neg;

(*----------------*)
PROCEDURE Conj(READONLY x:T):T=
VAR
  z:T;
BEGIN
  z.re:=      x.re;
  z.im:=R.Neg(x.im);
  RETURN z;
END Conj;


(*----------------*)
PROCEDURE Mul(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=R.Sub(R.Mul(x.re,y.re),R.Mul(x.im,y.im));
  z.im:=R.Add(R.Mul(x.im,y.re),R.Mul(x.re,y.im));
  RETURN z;
END Mul;

PROCEDURE DivScale(READONLY x,y:T;):T RAISES {Error} =
VAR
  denom : R.T;
BEGIN
  denom := R.Add(R.Mul(x.re,x.re),R.Mul(y.im,y.im));
  IF R.Equal (denom, R.Zero) THEN
    RAISE Error(Err.divide_by_zero);
  END;
  RETURN T{R.Div(x.re,denom),R.Div(x.im,denom)};
END DivScale;

(*-------------------*)
PROCEDURE Div(READONLY x,y : T) : T RAISES {Error} =
  BEGIN
    RETURN DivScale(Mul(x,Conj(y)),y);
  END Div;

(*-------------------*)
PROCEDURE Rec(READONLY x : T) : T RAISES {Error} =
  BEGIN
    RETURN DivScale(Conj(x),x);
  END Rec;

(*-------------------*)
PROCEDURE Square(READONLY x : T) : T =
  VAR
    z : T;
  BEGIN
    z.re := R.Sub(R.Mul(x.re,x.re), R.Mul(x.im,x.im));
    z.im := R.Mul(R.Mul(x.im,x.re), R.Two);
    RETURN z;
  END Square;

(*----------------*)
PROCEDURE Scale(READONLY x:T; y:R.T):T=
VAR
  z:T;
BEGIN
  z.re:=R.Mul(x.re,y);
  z.im:=R.Mul(x.im,y);
  RETURN z;
END Scale;



(*==========================*)
BEGIN
END ComplexBasic.
