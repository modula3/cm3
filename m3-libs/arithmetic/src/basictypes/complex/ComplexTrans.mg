GENERIC MODULE ComplexTrans(C,R,RT);
(*Copyright (x) 1996, m3na project

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
               READONLY x:T):R.T=
BEGIN
  RETURN RT.ArcTan2(x.im,x.re);
END Arg;

(*----------------*)
PROCEDURE PowR( 
                READONLY x:T;
                y:R.T):T=
VAR
  arg:R.T;
  abs:R.T;
BEGIN
  arg:=Arg(x);
  abs:=Abs(x);
  RETURN C.Scale(Exp(T{R.Zero,R.Mul(arg,y)}),abs);
END PowR;

(*----------------*)
PROCEDURE Pow( 
                x,y:T
                  ):T=
VAR
  z:T;
BEGIN
  z:=Ln(x);
  z:=C.Mul(y,z);
  z:=Exp(z);
  RETURN z;
END Pow;


(*----------------*)
PROCEDURE Exp( 
                READONLY x:T):T=
BEGIN
  RETURN C.Scale(T{RT.Cos(x.im),RT.Sin(x.im)},RT.Exp(x.re));
END Exp;

(*----------------*)
PROCEDURE Ln( 
                READONLY x:T):T=
VAR
  z:T;
BEGIN
  (*z.re:= R.Div(RT.Ln(AbsSqr(x)),R.Two);*)
  z.re:= RT.Ln(AbsSqr(x)) / R.Two;
  z.im:= Arg(x);
  RETURN z;
END Ln;

(*----------------*)
PROCEDURE Cos( 
                READONLY x:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  IF ABS(x.re) > FLOAT(18.0D0,R.T) OR ABS(x.im) > FLOAT(18.0D0,R.T) THEN
    RAISE Error(Err.out_of_range);
  END;
  z.re:=+RT.Cos(x.re)*RT.CosH(x.im);
  z.im:=-RT.Sin(x.re)*RT.SinH(x.im);
  RETURN z;
END Cos;
(*----------------*)
PROCEDURE Sin( 
                READONLY x:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  IF ABS(x.re) > FLOAT(18.0D0,R.T) OR ABS(x.im) > FLOAT(18.0D0,R.T) THEN
    RAISE Error(Err.out_of_range);
  END;
  z.re:=+RT.Sin(x.re)*RT.CosH(x.im);
  z.im:=+RT.Cos(x.re)*RT.SinH(x.im);
  RETURN z;
END Sin;
(*----------------*)
PROCEDURE Tan( 
                READONLY x:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  z:=C.Div(Sin(x),Cos(x));
  RETURN z;
END Tan;

(*----------------*)
PROCEDURE CosH( 
                READONLY x:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  z.re:=-x.im;
  z.im:=+x.re;
  z:=Cos(z);
  RETURN z;
END CosH;
(*----------------*)
PROCEDURE SinH( 
                READONLY x:T):T RAISES {Error}=
VAR
  z:T;
  t:R.T;
BEGIN
  z.re:=-x.im;
  z.im:=+x.re;
  z:=Sin(z);
  (*z.re = -i*i*z.im = z.im*)
  (*z.im = -i*z.re* = -z.re*)
  t:=z.im;
  z.im:=-z.re;
  z.re:=t;
  RETURN z;
END SinH;
(*----------------*)
PROCEDURE TanH( 
                READONLY x:T):T RAISES {Error}=
BEGIN
  RETURN C.Div(SinH(x),CosH(x));
END TanH;

(*-------------------*)
PROCEDURE Norm1(READONLY x : T) : R.T =
  BEGIN
    RETURN ABS(x.re) + ABS(x.im);
  END Norm1;

(*-------------------*)
PROCEDURE NormInf(READONLY x : T) : R.T =
  BEGIN
    RETURN MAX(ABS(x.re), ABS(x.im));
  END NormInf;

(* Lemming's stuff *)

PROCEDURE ArcSin (READONLY x : T) : T =
VAR
  ix, y : T;
BEGIN
  (* arcsin x = -i ln (ix ± sqrt (1-x²)) *)
  (*ix := C.Mul(x,C.I);*)
  ix := T{R.Neg(x.im),x.re};
  y  := SqRt(C.Add(C.Square(ix),C.One));
  RETURN C.Neg(C.Mul(C.I,Ln(C.Add(ix,y))));
END ArcSin;

PROCEDURE ArcCos (READONLY x : T) : T =
VAR
  y : T;
BEGIN
  (* arccos x = -i ln (x ± sqrt (x²-1)) *)
  y := SqRt(C.Sub(C.Square(x),C.One));
  RETURN C.Neg(C.Mul(C.I,Ln(C.Add(x,y))));
END ArcCos;

PROCEDURE ArcTan (READONLY x : T) : T RAISES{Error} =
VAR
  y : T;
BEGIN
  (* arctan x := 1/2i ln ((i-x)/(i+x)) *)
  y := C.Div(C.Sub(C.I,x),C.Add(C.I,x));
  RETURN C.Mul (Ln (y),T{R.Zero,RT.Half});
END ArcTan;


PROCEDURE Abs (READONLY x0 : T) : R.T =
VAR
  exp : INTEGER;
  x : T;
  y : R.T;
BEGIN
  (*a workaround to prevent NaNs and Zeros*)
  x := C.FrExp(x0, exp);
  y := RT.SqRt(AbsSqr(x));
  RETURN R.LdExp(y,exp);
END Abs;

(*
PROCEDURE Abs (READONLY x : T) : R.T =
BEGIN
  RETURN RT.SqRt(AbsSqr(x));
END Abs;
*)

PROCEDURE AbsSqr (READONLY x : T) : R.T =
BEGIN
(*  RETURN C.Mul(x,C.Conj(x));  it's a real number, but the type is still T *)
  RETURN x.re * x.re + x.im * x.im;
END AbsSqr;

PROCEDURE SqRt (READONLY x : T) : T =
VAR
  r : R.T;
  z : T;
BEGIN
  TRY
    r := Abs(x);
    z.re := R.Add (r, x.re);
    IF R.Compare(z.re, R.Zero) < 0 THEN (* mathematically impossible, can be caused by rounding *)
      z.re := R.Zero;
    ELSE
      z.re := RT.SqRt (R.Div(z.re,R.Two));
    END;

    z.im := R.Sub (r, x.re);
    IF R.Compare(z.im, R.Zero) < 0 THEN (* mathematically impossible, can be caused by rounding *)
      z.im := R.Zero;
    ELSE
      z.im := RT.SqRt (R.Div(z.im,R.Two));
      IF R.Compare(x.im, R.Zero) < 0 THEN  (* instead of using the Sgn function *)
        z.im := R.Neg (z.im);
      END;
    END;
    (* Root is on the same side as the radicand with respect to the real axis. *)

  EXCEPT
    Error(err) =>
      <*ASSERT err#Err.divide_by_zero*>
      (*division by zero never occurs, although Div() can throw it potentially*)
  END;
  RETURN z;
END SqRt;


(*==========================*)
BEGIN
END ComplexTrans.
