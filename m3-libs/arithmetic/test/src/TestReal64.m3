MODULE tReal64 EXPORTS test;
(*Copyright (c) 1995,1996 Harry George
Abstract: Test driver for numerical analysis

12/24/95  Harry George   Initial version
2/17/96   Harry George   Rebuilt into Real64

*)

IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;
(*=======================*)
CONST
  Module = "tReal64.";
VAR
(*=======================*)

(*----------------------*)
PROCEDURE test_ln_gamma():BOOLEAN=
CONST
  ftn = Module & "test_ln_gamma";
VAR
  result:=TRUE;
  ix,g:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 10 DO
    ix:=ix+10.0d0;
    g:=R.ln_gamma(ix);
    msg("ln_gamma(" & R.fmt(ix) & ")=" & R.fmt(g) & "\n");
  END;

  IF NOT verify(ftn,"ln_gamma",R.ln_gamma(10.0d0),
                             12.80182748008147D0,1.0d4) THEN
    result:=FALSE;
  END;
  RETURN result;
END test_ln_gamma;
(*----------------------*)
PROCEDURE test_betai():BOOLEAN=
CONST
  ftn = Module & "test_betai";
VAR
  result:=TRUE;
  ix,b:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+0.1d0;
    b:=R.betai(1.0d0,3.0d0,ix);
    msg("betai(1.0d0,3.0d0," & R.fmt(ix) & ")="
       &R.fmt(b) &  "\n");
  END;

  IF NOT verify(ftn,"betai",R.betai(1.0d0,3.0d0,0.2d0),0.488d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END test_betai;
(*----------------------*)
PROCEDURE test_gamma_p():BOOLEAN=
CONST
  ftn = Module & "test_gamma_p";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=R.gamma_p(a,ix);
    msg("gamma_p(" & R.fmt(a) & "," & R.fmt(ix) & ")="
       & R.fmt(y) &  "\n");
  END;

  IF NOT verify(ftn,"gamma_p",R.gamma_p(3.0d0,2.0d0),0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END test_gamma_p;
(*----------------------*)
PROCEDURE test_gamma_q():BOOLEAN=
CONST
  ftn = Module & "test_gamma_q";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:REAL64;
BEGIN
  debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=R.One-R.gamma_q(a,ix);
    msg("1-gamma_q(" & R.fmt(a) & "," & R.fmt(ix) & ")="
       & R.fmt(y) &  "\n");
  END;

  IF NOT verify(ftn,"gamma_q",R.gamma_q(3.0d0,2.0d0),
                            R.One-0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END test_gamma_q;
(*-------------------------*)
PROCEDURE test_Real64():BOOLEAN=
CONST ftn = Module & "test_Real64";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_ln_gamma();
  newline(); EVAL test_betai();
  newline(); EVAL test_gamma_p();
  newline(); EVAL test_gamma_q();
  RETURN result;
END test_Real64;
(*=======================*)
BEGIN
END tReal64.
