GENERIC MODULE PolarBasic(C,Ct,Rt);
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers in polar coordinates

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)

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
VAR
  c:C.T; 
BEGIN
  c.re:=p.radius*Rt.Cos(p.angle);
  c.im:=p.radius*Rt.Sin(p.angle);
  RETURN c;
END ToComplex;
(*----------------*)
PROCEDURE Mul( 
               READONLY p1,p2:T):T=
VAR
  tmp:T;
BEGIN
  tmp.radius:=p1.radius*p2.radius;
  tmp.angle :=p1.angle +p2.angle;

  (*---normalize to -pi..+pi---*)
  (*if it was normalized before,
    the loops should run at most one cycle*)
  WHILE tmp.angle > Rt.Pi DO
    tmp.angle:=tmp.angle-Rt.TwoPi;
  END;
  WHILE tmp.angle < -Rt.Pi DO
    tmp.angle:=tmp.angle+Rt.TwoPi;
  END;
  RETURN tmp;  
END Mul;
(*----------------*)
PROCEDURE Div( 
               READONLY p1,p2:T):T=
VAR
  tmp:T;
BEGIN
  tmp.radius:=p1.radius/p2.radius;
  tmp.angle :=p1.angle -p2.angle;
  (*---normalize to -pi..+pi---*)
  WHILE tmp.angle > Rt.Pi DO
    tmp.angle:=tmp.angle-Rt.TwoPi;
  END;
  WHILE tmp.angle < -Rt.Pi DO
    tmp.angle:=tmp.angle+Rt.TwoPi;
  END;
  RETURN tmp;  
END Div;


(*==========================*)
BEGIN
END PolarBasic.
