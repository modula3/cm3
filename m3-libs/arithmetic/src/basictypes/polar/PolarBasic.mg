GENERIC MODULE PolarBasic(C,Ct,R,Rt);
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers in polar coordinates

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)

FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "PolarBasic.";

(*----------------*)
PROCEDURE FromComplex( 
                   READONLY c:C.T):T=
VAR
  p:T;
BEGIN
  p.radius:=Ct.Abs(c);
  p.angle:=Rt.ArcTan2(c.im,c.re);
  RETURN p;
END FromComplex;

(*----------------*)
PROCEDURE ToComplex( 
                     READONLY p:T):C.T=
BEGIN
  RETURN C.Scale(C.T{re:=Rt.Cos(p.angle),im:=Rt.Sin(p.angle)},p.radius);
END ToComplex;
(*----------------*)
PROCEDURE NormalizeAngle(VAR p:T) =
  BEGIN
	(*---normalize to -pi..+pi---*)
	(*if it was normalized before,
      the loops should run at most one cycle*)
	WHILE R.Compare(p.angle,Rt.Pi)>0 DO
      p.angle:=R.Sub(p.angle,Rt.TwoPi);
	END;
	WHILE R.Compare(p.angle,R.Neg(Rt.Pi))<0 DO
      p.angle:=R.Add(p.angle,Rt.TwoPi);
	END;
  END NormalizeAngle;
(*----------------*)
PROCEDURE Mul( 
               READONLY p1,p2:T):T=
VAR
  tmp:T;
BEGIN
  tmp.radius:=R.Mul(p1.radius,p2.radius);
  tmp.angle :=R.Add(p1.angle, p2.angle);
  NormalizeAngle(tmp);
  RETURN tmp;  
END Mul;
(*----------------*)
PROCEDURE Div( 
               READONLY p1,p2:T):T RAISES {Error}=
VAR
  tmp:T;
BEGIN
  tmp.radius:=R.Div(p1.radius,p2.radius);
  tmp.angle :=R.Sub(p1.angle, p2.angle);
  NormalizeAngle(tmp);
  RETURN tmp;  
END Div;


(*==========================*)
BEGIN
END PolarBasic.
