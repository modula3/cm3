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

FROM xUtils IMPORT Error;

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
PROCEDURE IsZero(READONLY x:T):BOOLEAN =
BEGIN
  RETURN R.IsZero(x.re) AND R.IsZero(x.im);
END IsZero;

(*----------------*)
PROCEDURE Equal(READONLY x,y:T):BOOLEAN =
BEGIN
  RETURN R.Equal(x.re,y.re) AND R.Equal(x.im,y.im);
END Equal;


(*----------------*)
PROCEDURE Mul(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=R.Sub(R.Mul(x.re,y.re),R.Mul(x.im,y.im));
  z.im:=R.Add(R.Mul(x.im,y.re),R.Mul(x.re,y.im));
  RETURN z;
END Mul;

(*----------------*)
(*
Use Karatsuba's trick
tend to produce truncation errors

xr yr - xi yi
xr yi + xi yr

(xr - xi) (yr + yi) - xr yi + xi yr
*)
<*UNUSED*>
PROCEDURE FastMul(READONLY x,y:T):T=
VAR
  z:T;
  xyri, xryi, xiyr : R.T;
BEGIN
  xyri := R.Mul(R.Sub(x.re,x.im),R.Add(y.re,y.im));
  xryi := R.Mul(x.re,y.im);
  xiyr := R.Mul(x.im,y.re);
  z.re := R.Add(R.Sub(xyri,xryi),xiyr);
  z.im := R.Add(xryi,xiyr);
  RETURN z;
END FastMul;

PROCEDURE DivScale(READONLY x,y:T;):T RAISES {Error} =
VAR
  denom : R.T;
BEGIN
  denom := R.Add(R.Mul(x.re,x.re),R.Mul(y.im,y.im));
  (* Err.divide_by_zero will be thrown by Div*)
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
(*I have not found a measure which matches the needs
  of the Euclidean algorithm, it may work for Gaussian numbers though*)
PROCEDURE Mod(READONLY x,y:T):T RAISES {Error} =
  VAR
    denom : R.T;
    r     : T;
  BEGIN
    denom := R.Add(R.Mul(x.re,x.re),R.Mul(y.im,y.im));
    (* Err.divide_by_zero will be thrown by Mod*)
    r.re := R.Mod(x.re,denom);
    r.im := R.Mod(x.im,denom);
    r := Mul(r,Conj(y));  (*in fact, r is now AbsSqr(y) as big as before*)
    r.re := R.Div(r.re,denom);  (*is always divisible*)
    r.im := R.Div(r.im,denom);
    RETURN r;
  END Mod;

(*-------------------*)
PROCEDURE DivMod(READONLY x,y:T;VAR r:T):T RAISES {Error} =
  VAR
    denom : R.T;
    q     : T;
  BEGIN
    denom := R.Add(R.Mul(x.re,x.re),R.Mul(y.im,y.im));
    (* Err.divide_by_zero will be thrown by Div*)
    q.re := R.DivMod(x.re,denom,r.re);
    q.im := R.DivMod(x.im,denom,r.im);
    r := Mul(r,Conj(y));  (*in fact, r is now AbsSqr(y) as big as before*)
    r.re := R.Div(r.re,denom);  (*is always divisible*)
    r.im := R.Div(r.im,denom);
    RETURN q;
  END DivMod;


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

(*
PROCEDURE ScaleInt(x:T;y:INTEGER):T=
VAR
  z:T;
BEGIN
  z.re:=R.ScaleInt(x.re,y);
  z.im:=R.ScaleInt(x.im,y);
  RETURN z;
END ScaleInt;
*)

(*==========================*)
BEGIN
END ComplexBasic.
