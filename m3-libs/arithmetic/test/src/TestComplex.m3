MODULE TestComplex EXPORTS Test;
(*Copyright (c) 1996, Harry George
Abstract: Test driver for Complex

1/27/96  Harry George   Initial version

*)

IMPORT Fmt,
       LongRealComplexFast   AS C,
       LongRealComplexTrans  AS CT,
       LongRealComplexFmtLex AS CF,
       LongRealPolarBasic  AS P,
       LongRealPolarFmtLex AS PF;
(*=======================*)
CONST Module = "TestComplex.";

(*------------------------------*)
<*FATAL ANY*>
PROCEDURE TestComplex():BOOLEAN=
CONST ftn = Module & "TestComplex";
VAR
  a:=C.T{1.0D0,2.0D0};
  b:=C.T{-3.0D0,-4.0D0};
  p1,p2:P.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

    Msg("a=" & CF.Fmt(a) & "\n" &
        "b=" & CF.Fmt(b) & "\n");
    Msg("|a|   =" & Fmt.LongReal(CT.Abs(a)) & "\n");
    Msg("arg(a)=" & Fmt.LongReal(CT.Arg(a)) & "\n");
    Msg("conj a=" & CF.Fmt(C.Conj(a)) & "\n");
    Msg("a*2.0 =" & CF.Fmt(C.Scale(a,2.0D0)) & "\n");
    Msg("a+b   =" & CF.Fmt(C.Add(a,b)) & "\n");
    Msg("a+b   =" & CF.Fmt(C.Add(a,b)) & "\n");
    Msg("a-b   =" & CF.Fmt(C.Sub(a,b)) & "\n");
    Msg("a*b   =" & CF.Fmt(C.Mul(a,b)) & "\n");
    Msg("a/b   =" & CF.Fmt(C.Div(a,b)) & "\n");
    Msg("sqrt(a)=" & CF.Fmt(CT.SqRt(a)) & "\n");
    Msg("a^3.0="       & CF.Fmt(CT.PowR(a,3.0D0)) & "\n");
    Msg("a^(1.0/3.0)=" & CF.Fmt(CT.PowR(a,1.0D0/3.0D0)) & "\n");
    Msg("a^b="         & CF.Fmt(CT.Pow(a,b)) & "\n");
    Msg("exp(a)="      & CF.Fmt(CT.Exp(a)) & "\n");
    Msg("ln(a) ="      & CF.Fmt(CT.Ln(a)) & "\n");
    Msg("cos(a)="      & CF.Fmt(CT.Cos(a)) & "\n");
    Msg("sin(a)="      & CF.Fmt(CT.Sin(a)) & "\n");
    Msg("tan(a)="      & CF.Fmt(CT.Tan(a)) & "\n");
    Msg("cosh(a)="     & CF.Fmt(CT.CosH(a)) & "\n");
    Msg("sinh(a)="     & CF.Fmt(CT.SinH(a)) & "\n");
    Msg("tanh(a)="     & CF.Fmt(CT.TanH(a)) & "\n");

    NewLine();
    p1:=P.FromComplex(a); p2:=P.FromComplex(b);
    Msg("p1=toPolar(a)=" & PF.Fmt(P.FromComplex(a)) & "\n");
    Msg("fromPolar(p1) =" & CF.Fmt(P.ToComplex(p1)) & "\n");
    Msg("p2=toPolar(b)=" & PF.Fmt(P.FromComplex(b)) & "\n");
    Msg("fromPolar(p2) =" & CF.Fmt(P.ToComplex(p2)) & "\n");
    Msg("p1*p2 =" & PF.Fmt(P.Mul(p1,p2)) & "\n");
    Msg("p1/p2 =" & PF.Fmt(P.Div(p1,p2)) & "\n");

  RETURN result;
END TestComplex;

(*=======================*)
BEGIN
END TestComplex.
