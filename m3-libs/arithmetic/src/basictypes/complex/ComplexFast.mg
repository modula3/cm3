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

IMPORT FloatMode;
FROM NADefinitions IMPORT Error, Err;

<*UNUSED*> CONST Module = "ComplexFast.";
(*==========================*)

(*
  All routines with direct access to infix operators.
  May be useful as long as the compiler cannot handle INLINE procedures.
*)

(*--------------*)
PROCEDURE FromInteger(x:INTEGER):T =
BEGIN
  RETURN T{FLOAT(x,R.T),R.Zero};
END FromInteger;

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
PROCEDURE IsZero(READONLY x:T):BOOLEAN =
BEGIN
  RETURN x.re=R.Zero AND x.im=R.Zero;
END IsZero;

(*----------------*)
PROCEDURE Equal(READONLY x,y:T):BOOLEAN =
BEGIN
  RETURN x.re=y.re AND x.im=y.im;
END Equal;


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
PROCEDURE Div(READONLY x0,y0 : T) : T RAISES {Error} =
  VAR
    x, y, z : T;
    denom   : R.T;
    exp     : INTEGER;
  <*FATAL FloatMode.Trap*>
  BEGIN
    (*avoid overflow and underflow by conditioning*)
    x := Normalize(x0,exp);
    y := Scalb(y0,-exp);
    denom := y.re*y.re + y.im*y.im;
    IF denom=R.Zero THEN
      RAISE Error(Err.divide_by_zero);
    END;
    z.re := (  x.re * y.re + x.im * y.im) / denom;
    z.im := (- x.re * y.im + x.im * y.re) / denom;
    RETURN z;
  END Div;

(*-------------------*)
PROCEDURE Rec(READONLY x0 : T) : T RAISES{Error} =
  VAR
    x, z  : T;
    denom : R.T;
    exp   : INTEGER;
  <*FATAL FloatMode.Trap*>
  BEGIN
    x := Normalize(x0,exp);
    denom := x.re*x.re + x.im*x.im;
    IF denom=R.Zero THEN
      RAISE Error(Err.divide_by_zero);
    END;
    z.re :=  x.re / denom;
    z.im := -x.im / denom;
    RETURN Scalb(z,-exp);
  END Rec;

(*-------------------*)
PROCEDURE Mod(<*UNUSED*> READONLY x:T; READONLY y:T):T RAISES {Error} =
  BEGIN
    IF y.re=R.Zero AND y.im=R.Zero THEN
      RAISE Error(Err.divide_by_zero);
    END;
    RETURN Zero;
  END Mod;

(*-------------------*)
PROCEDURE DivMod(READONLY x,y:T): QuotRem RAISES {Error} =
  BEGIN
    RETURN QuotRem{Div(x,y),Zero};
  END DivMod;

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


(*-------------------*)

PROCEDURE Normalize (READONLY x: T; VAR exp: INTEGER): T =
  <*FATAL FloatMode.Trap*>
  BEGIN
    IF NOT IsZero(x) THEN
      exp:=ILogb(x);
      RETURN Scalb(x,-exp);
    ELSE
      (* ILogb(0)=-Infinity *)
      exp:=0;
      RETURN x;
    END;
  END Normalize;

(*
PROCEDURE Normalize (READONLY x: T; VAR exp: INTEGER): T =
  BEGIN
    exp:=0;
    IF x.re#R.Zero THEN exp:=exp+R.ILogb(x.re) END;
    IF x.im#R.Zero THEN exp:=exp+R.ILogb(x.im) END;
    exp := exp DIV 2;
    (*exp := ILogb (x);   ILogb(0)=-Infinity *)
    RETURN Scalb(x,-exp);
  END Normalize;
*)

PROCEDURE ILogb (READONLY x: T): INTEGER =
  BEGIN
    RETURN R.ILogb(ABS(x.re)+ABS(x.im)) DIV 2;
  END ILogb;

PROCEDURE Scalb (READONLY x: T; exp: INTEGER): T RAISES {FloatMode.Trap} =
  BEGIN
    RETURN T{R.Scalb(x.re,exp),R.Scalb(x.im,exp)};
  END Scalb;


(*==========================*)
BEGIN
END ComplexFast.
