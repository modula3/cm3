MODULE TestCmplx EXPORTS Test;
(*Copyright (c) 1996, Harry George
Abstract: Test driver for Complex

1/27/96  Harry George   Initial version

*)

IMPORT Fmt, xComplex AS C;
(*=======================*)
<*UNUSED*> CONST Module = "TestCmplx.";

(*------------------------------*)
PROCEDURE TestCmplx():BOOLEAN=
CONST ftn = Module & "TestCmplx";
VAR
  a:=C.COMPLEX{1.0D0,2.0D0};
  b:=C.COMPLEX{-3.0D0,-4.0D0};
  p1,p2:C.POLAR;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  
    Msg("a=" & C.fmt(a) & "\n" &
        "b=" & C.fmt(b) & "\n");
    Msg("|a|   =" & Fmt.LongReal(C.abs(a)) & "\n");
    Msg("arg(a)=" & Fmt.LongReal(C.arg(a)) & "\n");
    Msg("conj a=" & C.fmt(C.conj(a)) & "\n");
    Msg("a*2.0 =" & C.fmt(C.scale(a,2.0D0)) & "\n");
    Msg("a+b   =" & C.fmt(C.add(a,b)) & "\n");
    Msg("a+b   =" & C.fmt(C.add(a,b)) & "\n");
    Msg("a-b   =" & C.fmt(C.sub(a,b)) & "\n");
    Msg("a*b   =" & C.fmt(C.mul(a,b)) & "\n");
    Msg("a/b   =" & C.fmt(C.div(a,b)) & "\n");
    Msg("sqrt(a)=" & C.fmt(C.sqrt(a)) & "\n");
    Msg("a^3.0="       & C.fmt(C.powN(a,3.0D0)) & "\n");
    Msg("a^(1.0/3.0)=" & C.fmt(C.powN(a,1.0D0/3.0D0)) & "\n");
    Msg("a^b="         & C.fmt(C.powXY(a,b)) & "\n");
    Msg("exp(a)="      & C.fmt(C.exp(a)) & "\n");
    Msg("ln(a) ="      & C.fmt(C.ln(a)) & "\n");
    Msg("cos(a)="      & C.fmt(C.cos(a)) & "\n");
    Msg("sin(a)="      & C.fmt(C.sin(a)) & "\n");
    Msg("tan(a)="      & C.fmt(C.tan(a)) & "\n");
    Msg("cosh(a)="     & C.fmt(C.cosh(a)) & "\n");
    Msg("sinh(a)="     & C.fmt(C.sinh(a)) & "\n");
    Msg("tanh(a)="     & C.fmt(C.tanh(a)) & "\n");

    NewLine();
    p1:=C.toPolar(a); p2:=C.toPolar(b);
    Msg("p1=toPolar(a)=" & C.fmtPolar(C.toPolar(a)) & "\n");
    Msg("fromPolar(p1) =" & C.fmt(C.fromPolar(p1)) & "\n");
    Msg("p2=toPolar(b)=" & C.fmtPolar(C.toPolar(b)) & "\n");
    Msg("fromPolar(p2) =" & C.fmt(C.fromPolar(p2)) & "\n");
    Msg("p1*p2 =" & C.fmtPolar(C.pmul(p1,p2)) & "\n");
    Msg("p1/p2 =" & C.fmtPolar(C.pdiv(p1,p2)) & "\n");
    
  RETURN result;
END TestCmplx;

(*=======================*)
BEGIN
END TestCmplx.
