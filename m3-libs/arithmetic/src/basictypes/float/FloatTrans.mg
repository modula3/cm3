GENERIC MODULE FloatTrans();
(*Copyright (c) 1996, m3na project

Abstract: Generic wrapper routines for (mainly) transcendent functions

          It was xReal32.m3 by Harry George
          Initially implemented a wrappers on Math, but
          need an assembler rendition.

12/15/95  Harry George   Initial version, via wrappers on Math

*)

IMPORT Math;

<*UNUSED*> CONST Module = "FloatTrans.";
(*==========================*)

(*----------------*)
PROCEDURE Abs (x : T) : T =
BEGIN
  RETURN ABS(x);
END Abs;

(*----------------*)
PROCEDURE AbsSqr (x : T) : T =
BEGIN
  RETURN x*x;
END AbsSqr;

(*---- Exponential and Logarithm functions ----*)
(*----------------*)
PROCEDURE Exp (x: T): T=
(* returns E^x. *)
BEGIN
  RETURN FLOAT(Math.exp(FLOAT(x,LONGREAL)),T);
END Exp;

(*----------------*)
PROCEDURE Expm1 (x: T): T=
(* returns (E^x)-1, even for small x. *)
BEGIN
  RETURN FLOAT(Math.expm1(FLOAT(x,LONGREAL)),T);
END Expm1;

(*----------------*)
PROCEDURE Ln (x: T): T=
(* returns the natural logarithm of x (base E). *)
BEGIN
  RETURN FLOAT(Math.log(FLOAT(x,LONGREAL)),T);
END Ln;

(*----------------*)
PROCEDURE Ln1p (x: T): T=
(* returns log(1+x), even for small x. *)
BEGIN
  RETURN FLOAT(Math.log1p(FLOAT(x,LONGREAL)),T);
END Ln1p;

(*----------------*)
PROCEDURE Lg (x: T): T=
(* returns the base 10 logarithm of x. *)
BEGIN
  RETURN FLOAT(Math.log10(FLOAT(x,LONGREAL)),T);
END Lg;

(*----------------*)
PROCEDURE Pow (x, y: T): T=
(* returns x^y. *)
BEGIN
  RETURN FLOAT(Math.pow(FLOAT(x,LONGREAL),
                        FLOAT(y,LONGREAL)),T);
END Pow;

(*----------------*)
PROCEDURE SqRt (x: T): T=
(* returns the square root of x. *)
BEGIN
  RETURN FLOAT(Math.sqrt(FLOAT(x,LONGREAL)),T);
END SqRt;


(*---- Trigonometric functions ----*)

(*----------------*)
PROCEDURE Cos (x: T): T=
(* returns the cosine of x radians. *)
BEGIN
  RETURN FLOAT(Math.cos(FLOAT(x,LONGREAL)),T);
END Cos;

(*----------------*)
PROCEDURE Sin (x: T): T=
(* returns the sine of x radians. *)
BEGIN
  RETURN FLOAT(Math.sin(FLOAT(x,LONGREAL)),T);
END Sin;

(*----------------*)
PROCEDURE Tan (x: T): T=
(* returns the tangent of x radians. *)
BEGIN
  RETURN FLOAT(Math.tan(FLOAT(x,LONGREAL)),T);
END Tan;

(*----------------*)
PROCEDURE ArcCos (x: T): T=
(* returns the arc cosine of x in radians. *)
BEGIN
  RETURN FLOAT(Math.acos(FLOAT(x,LONGREAL)),T);
END ArcCos;

(*----------------*)
PROCEDURE ArcSin (x: T): T=
(* returns the arc sine of x in radians. *)
BEGIN
  RETURN FLOAT(Math.asin(FLOAT(x,LONGREAL)),T);
END ArcSin;

(*----------------*)
PROCEDURE ArcTan (x: T): T=
(* returns the arc tangent of x in radians. *)
BEGIN
  RETURN FLOAT(Math.atan(FLOAT(x,LONGREAL)),T);
END ArcTan;

(*----------------*)
PROCEDURE ArcTan2 (y, x: T): T=
(* returns the arc tangent of y/x in radians. *)
BEGIN
  RETURN FLOAT(Math.atan2(FLOAT(y,LONGREAL),
                          FLOAT(x,LONGREAL)),T);
END ArcTan2;


(*---- Hyperbolic trigonometric functions ----*)

(*----------------*)
PROCEDURE CosH (x: T): T=
(* returns the hyperbolic cosine of x. *)
BEGIN
  RETURN FLOAT(Math.cosh(FLOAT(x,LONGREAL)),T);
END CosH;

(*----------------*)
PROCEDURE SinH (x: T): T=
(* returns the hyperbolic sine of x. *)
BEGIN
  RETURN FLOAT(Math.sinh(FLOAT(x,LONGREAL)),T);
END SinH;

(*----------------*)
PROCEDURE TanH (x: T): T=
(* returns the hyperbolic tangent of x. *)
BEGIN
  RETURN FLOAT(Math.tanh(FLOAT(x,LONGREAL)),T);
END TanH;


(*----------------*)
PROCEDURE ArCosH (x: T): T=
(* returns the inverse hyperbolic cosine of x *)
BEGIN
  RETURN FLOAT(Math.acosh(FLOAT(x,LONGREAL)),T);
END ArCosH;

(*----------------*)
PROCEDURE ArSinH (x: T): T=
(* returns the inverse hyperbolic sine of x *)
BEGIN
  RETURN FLOAT(Math.asinh(FLOAT(x,LONGREAL)),T);
END ArSinH;

(*----------------*)
PROCEDURE ArTanH (x: T): T=
(* returns the inverse hyperbolic tangent of x *)
BEGIN
  RETURN FLOAT(Math.atanh(FLOAT(x,LONGREAL)),T);
END ArTanH;



PROCEDURE Sgn(x:T):T=
BEGIN
  IF x > Zero THEN
    RETURN One;
  ELSIF x < Zero THEN
    RETURN MinusOne;
  ELSE
    RETURN Zero;
  END;
END Sgn;


PROCEDURE FrExp (x: T; VAR exp: INTEGER): T =
  BEGIN
    RETURN FLOAT(Math.frexp(FLOAT(x,LONGREAL),exp),T);
  END FrExp;

PROCEDURE LdExp (x: T; exp: INTEGER): T =
  BEGIN
    RETURN FLOAT(Math.ldexp(FLOAT(x,LONGREAL),exp),T);
  END LdExp;

PROCEDURE ModF (x: T; VAR(*OUT*) i: T): T =
  VAR
    y, z : LONGREAL;
  BEGIN
    z := Math.modf(FLOAT(x,LONGREAL),y);
    i := FLOAT(y,T);
    RETURN FLOAT(z,T);
  END ModF;

BEGIN
END FloatTrans.
