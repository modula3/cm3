GENERIC MODULE ComplexFast(R);
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

<*UNUSED*> CONST Module = "ComplexFast.";
(*==========================*)

(*
  All routines with direct access to infix operators. 
  May be useful as long as the compiler cannot handle INLINE procedures.
*)

(*--------------*)
PROCEDURE Add(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=x.re+y.re;
  z.im:=x.im+y.im;
  RETURN z;
END Add;

(*--------------*)
PROCEDURE Sub(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=x.re-y.re;
  z.im:=x.im-y.im;
  RETURN z;
END Sub;

(*-------------------*)
PROCEDURE Neg(READONLY x : T) : T =
  VAR
    z : T;
  BEGIN
    z.re := -x.re;
    z.im := -x.im;
    RETURN z;
  END Neg;

(*----------------*)
PROCEDURE Conj(READONLY x:T):T=
VAR
  z:T;
BEGIN
  z.re:= x.re;
  z.im:=-x.im;
  RETURN z;
END Conj;


(*----------------*)
PROCEDURE Mul(READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.re:=x.re*y.re - x.im*y.im;
  z.im:=x.im*y.re + x.re*y.im;
  RETURN z;
END Mul;

(*-------------------*)
PROCEDURE Div(READONLY x,y : T) : T RAISES {Error} =
  VAR
    z : T;
    denom : R.T;
  BEGIN
    denom := y.re*y.re + y.im*y.im;
    IF denom=R.Zero THEN
	  RAISE Error(Err.divide_by_zero);
	END;
    z.re := (  x.re * y.re + x.im * y.im) / denom;
    z.im := (- x.re * y.im + x.im * y.re) / denom;
    RETURN z;
  END Div;

(*-------------------*)
PROCEDURE Rec(READONLY x : T) : T RAISES{Error} =
  VAR
    z : T;
    denom : R.T;
  BEGIN
    denom := x.re*x.re + x.im*x.im;
    IF denom=R.Zero THEN
	  RAISE Error(Err.divide_by_zero);
	END;
    z.re :=  x.re / denom;
    z.im := -x.im / denom;
    RETURN z;
  END Rec;

(*-------------------*)
PROCEDURE Square(READONLY x : T) : T =
  VAR
    z : T;
  BEGIN
    z.re := x.re * x.re - x.im * x.im;
    z.im := x.im * x.re * FLOAT(2.0,R.T);
    RETURN z;
  END Square;

(*----------------*)
PROCEDURE Scale(READONLY x:T; y:R.T):T=
VAR
  z:T;
BEGIN
  z.re:=y*x.re;
  z.im:=y*x.im;
  RETURN z;
END Scale;

(*----------------*)
PROCEDURE ScaleInt(x:T;y:INTEGER):T=
VAR
  z:T;
BEGIN
  z.re:=x.re*FLOAT(y,R.T);
  z.im:=x.im*FLOAT(y,R.T);
  RETURN z;
END ScaleInt;

(*==========================*)
BEGIN
END ComplexFast.
