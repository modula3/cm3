MODULE tCmplx EXPORTS test;
(*Copyright (c) 1996, Harry George
Abstract: Test driver for Complex

1/27/96  Harry George   Initial version

*)

IMPORT Fmt, xComplex AS C;
(*=======================*)
<*UNUSED*> CONST Module = "tCmplx.";

(*------------------------------*)
PROCEDURE test_Cmplx():BOOLEAN=
CONST ftn = Module & "test_Cmplx";
VAR
  a:=C.COMPLEX{1.0D0,2.0D0};
  b:=C.COMPLEX{-3.0D0,-4.0D0};
  p1,p2:C.POLAR;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  
    msg("a=" & C.fmt(a) & "\n" &
        "b=" & C.fmt(b) & "\n");
    msg("|a|   =" & Fmt.LongReal(C.abs(a)) & "\n");
    msg("arg(a)=" & Fmt.LongReal(C.arg(a)) & "\n");
    msg("conj a=" & C.fmt(C.conj(a)) & "\n");
    msg("a*2.0 =" & C.fmt(C.scale(a,2.0D0)) & "\n");
    msg("a+b   =" & C.fmt(C.add(a,b)) & "\n");
    msg("a+b   =" & C.fmt(C.add(a,b)) & "\n");
    msg("a-b   =" & C.fmt(C.sub(a,b)) & "\n");
    msg("a*b   =" & C.fmt(C.mul(a,b)) & "\n");
    msg("a/b   =" & C.fmt(C.div(a,b)) & "\n");
    msg("sqrt(a)=" & C.fmt(C.sqrt(a)) & "\n");
    msg("a^3.0="       & C.fmt(C.powN(a,3.0D0)) & "\n");
    msg("a^(1.0/3.0)=" & C.fmt(C.powN(a,1.0D0/3.0D0)) & "\n");
    msg("a^b="         & C.fmt(C.powXY(a,b)) & "\n");
    msg("exp(a)="      & C.fmt(C.exp(a)) & "\n");
    msg("ln(a) ="      & C.fmt(C.ln(a)) & "\n");
    msg("cos(a)="      & C.fmt(C.cos(a)) & "\n");
    msg("sin(a)="      & C.fmt(C.sin(a)) & "\n");
    msg("tan(a)="      & C.fmt(C.tan(a)) & "\n");
    msg("cosh(a)="     & C.fmt(C.cosh(a)) & "\n");
    msg("sinh(a)="     & C.fmt(C.sinh(a)) & "\n");
    msg("tanh(a)="     & C.fmt(C.tanh(a)) & "\n");

    newline();
    p1:=C.toPolar(a); p2:=C.toPolar(b);
    msg("p1=toPolar(a)=" & C.fmtPolar(C.toPolar(a)) & "\n");
    msg("fromPolar(p1) =" & C.fmt(C.fromPolar(p1)) & "\n");
    msg("p2=toPolar(b)=" & C.fmtPolar(C.toPolar(b)) & "\n");
    msg("fromPolar(p2) =" & C.fmt(C.fromPolar(p2)) & "\n");
    msg("p1*p2 =" & C.fmtPolar(C.pmul(p1,p2)) & "\n");
    msg("p1/p2 =" & C.fmtPolar(C.pdiv(p1,p2)) & "\n");
    
  RETURN result;
END test_Cmplx;

(*=======================*)
BEGIN
END tCmplx.
