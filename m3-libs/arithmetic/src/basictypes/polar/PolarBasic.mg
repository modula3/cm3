GENERIC MODULE PolarBasic(R,RT,C,CT);
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers in polar coordinates

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)

FROM NADefinitions IMPORT Error;

<*UNUSED*> CONST Module = "PolarBasic.";

(*----------------*)
PROCEDURE IsZero(READONLY x:T):BOOLEAN =
BEGIN
  RETURN R.IsZero(x.radius);
END IsZero;

(*----------------*)
PROCEDURE Equal(READONLY x,y:T):BOOLEAN =
BEGIN
  RETURN R.Equal(x.radius,y.radius) AND R.Equal(x.angle,y.angle);
END Equal;

(*----------------*)
PROCEDURE FromComplex(
                   READONLY c:C.T):T=
VAR
  x:T;
BEGIN
  x.radius:=CT.Abs(c);
  x.angle:=RT.ArcTan2(c.im,c.re);
  RETURN x;
END FromComplex;

(*----------------*)
PROCEDURE ToComplex(
                     READONLY x:T):C.T=
BEGIN
  RETURN C.Scale(C.T{re:=RT.Cos(x.angle),im:=RT.Sin(x.angle)},x.radius);
END ToComplex;
(*----------------*)
PROCEDURE NormalizeAngle(VAR x:T) =
  BEGIN
	(*---normalize to -pi..+pi---*)
	(*if it was normalized before,
      the loops should run at most one cycle*)
	WHILE R.Compare(x.angle,RT.Pi)>0 DO
      x.angle:=R.Sub(x.angle,RT.TwoPi);
	END;
	WHILE R.Compare(x.angle,R.Neg(RT.Pi))<0 DO
      x.angle:=R.Add(x.angle,RT.TwoPi);
	END;
  END NormalizeAngle;
(*----------------*)
PROCEDURE Mul(
               READONLY x,y:T):T=
VAR
  z:T;
BEGIN
  z.radius:=R.Mul(x.radius,y.radius);
  z.angle :=R.Add(x.angle, y.angle);
  NormalizeAngle(z);
  RETURN z;
END Mul;
(*----------------*)
PROCEDURE Div(
               READONLY x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  z.radius:=R.Div(x.radius,y.radius);
  z.angle :=R.Sub(x.angle, y.angle);
  NormalizeAngle(z);
  RETURN z;
END Div;


(*==========================*)
BEGIN
END PolarBasic.
