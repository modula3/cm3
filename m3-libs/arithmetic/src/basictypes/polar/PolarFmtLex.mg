GENERIC MODULE PolarFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Complex numbers and associated functions

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)
IMPORT Fmt,xReal64 AS R;
FROM xUtils IMPORT Error,Err;
FROM xReal64 IMPORT REAL64;

<*UNUSED*> CONST Module = "xComplex.";

(*------------------------*)
<*UNUSED*>
PROCEDURE lex( 
               str:TEXT):COMPLEX RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END lex;
(*----------------------------*)
PROCEDURE fmt ( 
                VALUE c:COMPLEX;
                style:Fmt.Style:=Fmt.Style.Fix;
                prec:CARDINAL:=3
                ):TEXT=
(*Generate in format "COMPLEX{re:=1.0,im:=2.0}"
Uses simple Fmt.Real if c.im=0.0.
style and precision can be overridden*)
VAR
  t:TEXT;
BEGIN
  IF c.im=R.Zero THEN
    t:=R.fmt(c.re);
    RETURN t;
  END;
  t:="COMPLEX{re:=" & R.fmt(c.re,style,prec) & "D0,"
           & "im:=" & R.fmt(c.im,style,prec) & "D0}";
  RETURN t;
END fmt;
(*----------------*)
PROCEDURE arg( 
               VALUE c:COMPLEX):REAL64=
BEGIN
  RETURN R.atan2(c.im,c.re);
END arg;
(*----------------*)
PROCEDURE conj( 
                VALUE c:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:= c.re;
  tmp.im:=-c.im;
  RETURN tmp;
END conj;

(*--------------*)
PROCEDURE add( 
               c1,c2:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:=c1.re+c2.re;
  tmp.im:=c1.im+c2.im;
  RETURN tmp;
END add;
(*--------------*)
PROCEDURE sub( 
               c1,c2:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:=c1.re-c2.re;
  tmp.im:=c1.im-c2.im;
  RETURN tmp;
END sub;

(*----------------*)
PROCEDURE mul( 
               c1,c2:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:=c1.re*c2.re - c1.im*c2.im;
  tmp.im:=c1.im*c2.re + c1.re*c2.im;
  RETURN tmp;
END mul;


(*----------------*)
<*UNUSED*>
PROCEDURE hgg_div( 
               c1,c2:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
  ratio,den:REAL64;
BEGIN
  IF ABS(c2.re) > ABS(c2.im) THEN
    ratio:=c2.im/c2.re;
    den:=c2.re + c2.im*ratio;
    tmp.re:=(c1.re + c1.im*ratio)/den;
    tmp.im:=(c1.im - c1.re*ratio)/den;
  ELSE
    ratio:=c2.re/c2.im;
    den:=c2.im + c2.re*ratio;
    tmp.re:=(c1.re*ratio + c1.im)/den;
    tmp.im:=(c1.im*ratio - c1.re)/den;
  END;
  RETURN tmp;
END hgg_div;

(*----------------*)
PROCEDURE scale( 
                 VALUE c:COMPLEX;
                 x:REAL64
                 ):COMPLEX=
(*do row multiply, as if a is a row vector*)
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:=x*c.re;
  tmp.im:=x*c.im;
  RETURN tmp;
END scale;

(*----------------*)
PROCEDURE powN( 
                VALUE c:COMPLEX;
                n:REAL64):COMPLEX=
VAR
  p:POLAR;
BEGIN
  p:=toPolar(c);
  p.radius:=R.pow(p.radius,n);
  p.angle:=p.angle*n;
  RETURN fromPolar(p);
END powN;

(*----------------*)
PROCEDURE powXY( 
                x,y:COMPLEX
                  ):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp:=ln(x);
  tmp:=mul(y,tmp);
  tmp:=exp(tmp);
  RETURN tmp;
END powXY;


(*----------------*)
PROCEDURE exp( 
                VALUE c:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
  ex:REAL64;
BEGIN
  ex:=R.exp(c.re);
  tmp.re:= ex*R.cos(c.im);
  tmp.im:= ex*R.sin(c.im);
  RETURN tmp;
END exp;

(*----------------*)
PROCEDURE ln( 
                VALUE c:COMPLEX):COMPLEX=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:= R.log(abs(c));
  tmp.im:= arg(c);
  RETURN tmp;
END ln;

(*----------------*)
PROCEDURE cos( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
VAR
  tmp:COMPLEX;
BEGIN
  IF ABS(c.re) > 18.0D0 OR ABS(c.im) > 18.0D0 THEN
    RAISE Error(Err.out_of_range);
  END;
  tmp.re:=+R.cos(c.re)*R.cosh(c.im);
  tmp.im:=-R.sin(c.re)*R.sinh(c.im);
  RETURN tmp;
END cos;
(*----------------*)
PROCEDURE sin( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
VAR
  tmp:COMPLEX;
BEGIN
  IF ABS(c.re) > 18.0D0 OR ABS(c.im)> 18.0D0 THEN
    RAISE Error(Err.out_of_range);
  END;
  tmp.re:=+R.sin(c.re)*R.cosh(c.im);
  tmp.im:=+R.cos(c.re)*R.sinh(c.im);
  RETURN tmp;
END sin;
(*----------------*)
PROCEDURE tan( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
VAR
  tmp:COMPLEX;
BEGIN
  tmp:=div(sin(c),cos(c));
  RETURN tmp;
END tan;

(*----------------*)
PROCEDURE cosh( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
VAR
  tmp:COMPLEX;
BEGIN
  tmp.re:=-c.im;
  tmp.im:=+c.re;
  tmp:=cos(tmp);
  RETURN tmp;
END cosh;
(*----------------*)
PROCEDURE sinh( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
VAR
  tmp:COMPLEX;
  t:REAL64;
BEGIN
  tmp.re:=-c.im;
  tmp.im:=+c.re;
  tmp:=sin(tmp);
  (*tmp.re = -i*i*tmp.im = tmp.im*)
  (*tmp.im = -i*tmp.re* = -tmp.re*)
  t:=tmp.im;
  tmp.im:=-tmp.re;
  tmp.re:=t;
  RETURN tmp;
END sinh;
(*----------------*)
PROCEDURE tanh( 
                VALUE c:COMPLEX):COMPLEX RAISES {Error}=
BEGIN
  RETURN div(sinh(c),cosh(c));
END tanh;

      
(*---polar format---*)
(*----------------*)
PROCEDURE toPolar( 
                   VALUE c:COMPLEX):POLAR=
VAR
  p:POLAR;
BEGIN
  p.radius:=abs(c);
  p.angle:=R.atan2(c.im,c.re);
  RETURN p;
END toPolar;

(*----------------*)
PROCEDURE fromPolar( 
                     p:POLAR):COMPLEX=
VAR
  c:COMPLEX; 
BEGIN
  c.re:=p.radius*R.cos(p.angle);
  c.im:=p.radius*R.sin(p.angle);
  RETURN c;
END fromPolar;
(*----------------*)
PROCEDURE fmtPolar( 
                p:POLAR;
                style:Fmt.Style:=Fmt.Style.Fix;
                prec:CARDINAL:=3
                ):TEXT=
VAR
  t:TEXT;
BEGIN
  t:="POLAR{radius:=" & Fmt.LongReal(p.radius,style,prec) & "D0,"
         & "angle:="  & Fmt.LongReal(p.angle ,style,prec) & "D0}";
  RETURN t;
END fmtPolar;
(*----------------*)
PROCEDURE pmul( 
               p1,p2:POLAR):POLAR=
VAR
  tmp:POLAR;
BEGIN
  tmp.radius:=p1.radius*p2.radius;
  tmp.angle :=p1.angle +p2.angle;
  
  (*---normalize to -pi..+pi---*)
  WHILE tmp.angle > R.Pi DO
    tmp.angle:=tmp.angle-R.TwoPi;
  END;
  WHILE tmp.angle < -R.Pi DO
    tmp.angle:=tmp.angle+R.TwoPi;
  END;
  RETURN tmp;  
END pmul;
(*----------------*)
PROCEDURE pdiv( 
               p1,p2:POLAR):POLAR=
VAR
  tmp:POLAR;
BEGIN
  tmp.radius:=p1.radius/p2.radius;
  tmp.angle :=p1.angle -p2.angle;
  (*---normalize to -pi..+pi---*)
  WHILE tmp.angle > R.Pi DO
    tmp.angle:=tmp.angle-R.TwoPi;
  END;
  WHILE tmp.angle < -R.Pi DO
    tmp.angle:=tmp.angle+R.TwoPi;
  END;
  RETURN tmp;  
END pdiv;

(*===============*)
(* WDS Functions *)
(*===============*)
CONST RootHalf = 0.707106781186547524400844362104849039284835937689D0;

(*-------------------*)
PROCEDURE Magnitude(VALUE c : T) : LONGREAL =
  VAR
    x,y : LONGREAL;
  BEGIN
    x := ABS(c.re);
    y := ABS(c.im);
    IF x<=0.0D0 THEN
      RETURN y;
    ELSIF y<=0.0D0 THEN
      RETURN x;
    ELSIF x<=y THEN
      WITH q = x/y DO
        RETURN R.sqrt(q*q + 1.0D0) * y;
      END;
    ELSE (* x>y *)
      WITH q = y/x DO
        RETURN R.sqrt(q*q + 1.0D0) * x;
      END;
    END;
  END Magnitude;

(*-------------------*)
PROCEDURE FromRadiusAndTheta(r, theta : LONGREAL) : T =
  VAR
    c : T;
  BEGIN
    c.re := r * R.cos(theta);    
    c.im := r * R.sin(theta);    
    RETURN c;
  END FromRadiusAndTheta;

(*-------------------*)
PROCEDURE L1Norm(VALUE c : T) : LONGREAL =
  BEGIN
    RETURN ABS(c.re) + ABS(c.im);
  END L1Norm;

PROCEDURE LinfinityNorm(VALUE c : T) : LONGREAL =
  BEGIN
    RETURN MAX(ABS(c.re), ABS(c.im));
  END LinfinityNorm;

(*-------------------*)
PROCEDURE Negative(VALUE a : T) : T =
  VAR
    c : T;
  BEGIN
    c.re := -a.re;
    c.im := -a.im;
    RETURN c;
  END Negative;

(*-------------------*)
PROCEDURE Square(VALUE a : T) : T =
  VAR
    c : T;
  BEGIN
    c.re := a.re * a.re - a.im * a.im;
    c.im := a.im * a.re * 2.0D0;
    RETURN c;
  END Square;

(*-------------------*)
PROCEDURE Divide(VALUE a,b : T) : T =
  VAR
    c : T;
    q, denom: LONGREAL;
  BEGIN
    IF ABS(b.re) < ABS(b.im) THEN
      q := b.re / b.im;
      denom := b.im + q * b.re;
      <* ASSERT denom # 0.0D0 *>
      c.re := (q * a.re + a.im)/denom;
      c.im := (q * a.im - a.re)/denom;
    ELSE
      <* ASSERT b.re # 0.0D0 *>
      q := b.im / b.re;
      denom := b.re + q * b.im;
      <* ASSERT denom # 0.0D0 *>
      c.re := (a.re + q * a.im)/denom;
      c.im := (a.im - q * a.re)/denom;
    END;
    RETURN c;
  END Divide;

(*-------------------*)
(* Branch chosen so real part is always >= 0: *)
PROCEDURE Sqrt(VALUE a : T) : T =
  VAR
    x, y, q, mag : LONGREAL;
    c : T;
  BEGIN
    x := ABS(a.re);
    y := ABS(a.im);
    IF x < y THEN
      q := x/y;
      mag := y * R.sqrt(1.0D0 + q*q);
    ELSE
      IF y = 0.0D0 THEN
        q := R.sqrt(x);
        IF a.re >= 0.0D0 THEN
          c.re := q;  c.im := 0.0D0;
        ELSE
          c.re := 0.0D0;  c.im := q;
        END;
        RETURN c;
      END;
      <* ASSERT x > 0.0D0 *>
      q := y/x;
      mag := x * R.sqrt(1.0D0 + q*q);
    END;
    q := R.sqrt(x + mag);
    <* ASSERT q > 0.0D0 *>
    IF a.re>=0.0D0 THEN
      c.re := q * RootHalf;
      c.im := a.im * RootHalf / q;
    ELSE
      c.re := y * RootHalf / q;
      IF a.im < 0.0D0 THEN (* copysign?? *)
        c.im := -RootHalf * q;
      ELSE
        c.im :=  RootHalf * q;
      END; 
    END;
    RETURN c;
  END Sqrt;


(*==========================*)
BEGIN
END PolarFmtLex.
