GENERIC MODULE ComplexTrans(C,R,Rt);
(*Copyright (c) 1996, m3na project

Abstract: Transcendental functions of complex numbers.

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)
FROM xUtils IMPORT Error,Err;

<*UNUSED*> CONST Module = "ComplexTrans.";

(*----------------*)
PROCEDURE Arg( 
               READONLY c:C.T):R.T=
BEGIN
  RETURN Rt.ArcTan2(c.im,c.re);
END Arg;

(*----------------*)
PROCEDURE PowR( 
                READONLY c:C.T;
                y:R.T):C.T=
VAR
  arg:R.T;
  abs:R.T;
BEGIN
  arg:=Arg(c);
  abs:=Abs(c);
  RETURN C.Scale(Exp(C.T{R.Zero,R.Mul(arg,y)}),abs);
END PowR;

(*----------------*)
PROCEDURE Pow( 
                x,y:C.T
                  ):C.T=
VAR
  tmp:C.T;
BEGIN
  tmp:=Ln(x);
  tmp:=C.Mul(y,tmp);
  tmp:=Exp(tmp);
  RETURN tmp;
END Pow;


(*----------------*)
PROCEDURE Exp( 
                READONLY c:C.T):C.T=
VAR
  tmp:C.T;
  ex:R.T;
BEGIN
  ex:=Rt.Exp(c.re);
  tmp.re:= ex*Rt.Cos(c.im);
  tmp.im:= ex*Rt.Sin(c.im);
  RETURN tmp;
END Exp;

(*----------------*)
PROCEDURE Ln( 
                READONLY c:C.T):C.T=
VAR
  tmp:C.T;
BEGIN
  tmp.re:= Rt.Ln(Abs(c));
  tmp.im:= Arg(c);
  RETURN tmp;
END Ln;

(*----------------*)
PROCEDURE Cos( 
                READONLY c:C.T):C.T RAISES {Error}=
VAR
  tmp:C.T;
BEGIN
  IF ABS(c.re) > FLOAT(18.0D0,R.T) OR ABS(c.im) > FLOAT(18.0D0,R.T) THEN
    RAISE Error(Err.out_of_range);
  END;
  tmp.re:=+Rt.Cos(c.re)*Rt.CosH(c.im);
  tmp.im:=-Rt.Sin(c.re)*Rt.SinH(c.im);
  RETURN tmp;
END Cos;
(*----------------*)
PROCEDURE Sin( 
                READONLY c:C.T):C.T RAISES {Error}=
VAR
  tmp:C.T;
BEGIN
  IF ABS(c.re) > FLOAT(18.0D0,R.T) OR ABS(c.im) > FLOAT(18.0D0,R.T) THEN
    RAISE Error(Err.out_of_range);
  END;
  tmp.re:=+Rt.Sin(c.re)*Rt.CosH(c.im);
  tmp.im:=+Rt.Cos(c.re)*Rt.SinH(c.im);
  RETURN tmp;
END Sin;
(*----------------*)
PROCEDURE Tan( 
                READONLY c:C.T):C.T RAISES {Error}=
VAR
  tmp:C.T;
BEGIN
  tmp:=C.Div(Sin(c),Cos(c));
  RETURN tmp;
END Tan;

(*----------------*)
PROCEDURE CosH( 
                READONLY c:C.T):C.T RAISES {Error}=
VAR
  tmp:C.T;
BEGIN
  tmp.re:=-c.im;
  tmp.im:=+c.re;
  tmp:=Cos(tmp);
  RETURN tmp;
END CosH;
(*----------------*)
PROCEDURE SinH( 
                READONLY c:C.T):C.T RAISES {Error}=
VAR
  tmp:C.T;
  t:R.T;
BEGIN
  tmp.re:=-c.im;
  tmp.im:=+c.re;
  tmp:=Sin(tmp);
  (*tmp.re = -i*i*tmp.im = tmp.im*)
  (*tmp.im = -i*tmp.re* = -tmp.re*)
  t:=tmp.im;
  tmp.im:=-tmp.re;
  tmp.re:=t;
  RETURN tmp;
END SinH;
(*----------------*)
PROCEDURE TanH( 
                READONLY c:C.T):C.T RAISES {Error}=
BEGIN
  RETURN C.Div(SinH(c),CosH(c));
END TanH;

(*===============*)
(* WDS Functions *)
(*===============*)

(*-------------------*)
<*UNUSED*>
PROCEDURE Magnitude(READONLY c : C.T) : R.T =
  VAR
    x,y : R.T;
  BEGIN
    x := ABS(c.re);
    y := ABS(c.im);
    IF x<=R.Zero THEN
      RETURN y;
    ELSIF y<=R.Zero THEN
      RETURN x;
    ELSIF x<=y THEN
      WITH q = x/y DO
        RETURN Rt.SqRt(q*q + R.One) * y;
      END;
    ELSE (* x>y *)
      WITH q = y/x DO
        RETURN Rt.SqRt(q*q + R.One) * x;
      END;
    END;
  END Magnitude;

(*-------------------*)
PROCEDURE Norm1(READONLY c : C.T) : R.T =
  BEGIN
    RETURN ABS(c.re) + ABS(c.im);
  END Norm1;

PROCEDURE NormInf(READONLY c : C.T) : R.T =
  BEGIN
    RETURN MAX(ABS(c.re), ABS(c.im));
  END NormInf;

(*-------------------*)
(* Branch chosen so real part is always >= 0: *)
<*UNUSED*>
PROCEDURE SqrtWDS(READONLY a : C.T) : C.T =
  CONST
    RootHalf = FLOAT(0.707106781186547524400844362104849039284835937689D0,R.T);
  VAR
    x, y, q, mag : R.T;
    c : C.T;
  BEGIN
    x := ABS(a.re);
    y := ABS(a.im);
    IF x < y THEN
      q := x/y;
      mag := y * Rt.SqRt(R.One + q*q);
    ELSE
      IF y = R.Zero THEN
        q := Rt.SqRt(x);
        IF a.re >= R.Zero THEN
          c.re := q;  c.im := R.Zero;
        ELSE
          c.re := R.Zero;  c.im := q;
        END;
        RETURN c;
      END;
      <* ASSERT x > R.Zero *>
      q := y/x;
      mag := x * Rt.SqRt(R.One + q*q);
    END;
    q := Rt.SqRt(x + mag);
    <* ASSERT q > R.Zero *>
    IF a.re>=R.Zero THEN
      c.re := q * RootHalf;
      c.im := a.im * RootHalf / q;
    ELSE
      c.re := y * RootHalf / q;
      IF a.im < R.Zero THEN (* copysign?? *)
        c.im := -RootHalf * q;
      ELSE
        c.im :=  RootHalf * q;
      END; 
    END;
    RETURN c;
  END SqrtWDS;

(* Lemming's stuff *)

PROCEDURE ArcSin (READONLY x : C.T) : C.T =
VAR
  ix, y : C.T;
BEGIN
  (* arcsin x = -i ln (ix ± sqrt (1-x²)) *)
  ix := C.Mul(x,C.I);
  y  := SqRt(C.Add(C.Square(ix),C.One));
  RETURN C.Neg(C.Mul(C.I,Ln(C.Add(ix,y))));
END ArcSin;

PROCEDURE ArcCos (READONLY x : C.T) : C.T =
VAR
  y : C.T;
BEGIN
  (* arccos x = -i ln (x ± sqrt (x²-1)) *)
  y := SqRt(C.Sub(C.Square(x),C.One));
  RETURN C.Neg(C.Mul(C.I,Ln(C.Add(x,y))));
END ArcCos;

PROCEDURE ArcTan (READONLY x : C.T) : C.T =
VAR
  y : C.T;
BEGIN
  (* arctan x := 1/2i ln ((i-x)/(i+x)) *)
  y := C.Div(C.Sub(C.I,x),C.Add(C.I,x));
  RETURN C.Mul (Ln (y),C.T{R.Zero,Rt.Half});
END ArcTan;



PROCEDURE Abs (READONLY x : C.T) : R.T =
BEGIN
  RETURN Rt.SqRt(AbsSqr(x));
END Abs;

PROCEDURE AbsSqr (READONLY x : C.T) : R.T =
BEGIN
  RETURN x.re * x.re + x.im * x.im;
END AbsSqr;

PROCEDURE SqRt (READONLY x : C.T) : C.T =
VAR
  r : R.T;
  z : C.T;
BEGIN
  r := Abs(x);
  z.re := r + x.re;
  IF z.re < R.Zero THEN (* mathematically impossible, can be caused by rounding *)
    z.re := R.Zero;
  ELSE
    z.re := Rt.SqRt (R.Div(z.re,R.Two));
  END;

  z.im := r - x.re;
  IF z.im < R.Zero THEN (* mathematically impossible, can be caused by rounding *)
    z.im := R.Zero;
  ELSIF x.im >= R.Zero THEN  (* instead of using the Sgn function *)
    z.im :=        Rt.SqRt (R.Div(z.im,R.Two));
  ELSE
    z.im := R.Neg (Rt.SqRt (R.Div(z.im,R.Two)));
  END;
  (* Root is on the same side as the radicand with respect to the real axis. *)
  RETURN z;
END SqRt;

(*==========================*)
BEGIN
END ComplexTrans.
