MODULE TestReal64 EXPORTS Test;
(*Copyright (c) 1995,1996 Harry George
Abstract: Test driver for numerical analysis

12/24/95  Harry George   Initial version
2/17/96   Harry George   Rebuilt into Real64

*)

IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;
(*=======================*)
CONST
  Module = "TestReal64.";
VAR
(*=======================*)

(*----------------------*)
PROCEDURE TestLn_gamma():BOOLEAN=
CONST
  ftn = Module & "TestLn_gamma";
VAR
  result:=TRUE;
  ix,g:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 10 DO
    ix:=ix+10.0d0;
    g:=R.ln_gamma(ix);
    Msg("ln_gamma(" & R.fmt(ix) & ")=" & R.fmt(g) & "\n");
  END;

  IF NOT Verify(ftn,"ln_gamma",R.ln_gamma(10.0d0),
                             12.80182748008147D0,1.0d4) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestLn_gamma;
(*----------------------*)
PROCEDURE TestBetai():BOOLEAN=
CONST
  ftn = Module & "TestBetai";
VAR
  result:=TRUE;
  ix,b:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+0.1d0;
    b:=R.betai(1.0d0,3.0d0,ix);
    Msg("betai(1.0d0,3.0d0," & R.fmt(ix) & ")="
       &R.fmt(b) &  "\n");
  END;

  IF NOT Verify(ftn,"betai",R.betai(1.0d0,3.0d0,0.2d0),0.488d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestBetai;
(*----------------------*)
PROCEDURE TestGamma_p():BOOLEAN=
CONST
  ftn = Module & "TestGamma_p";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=R.gamma_p(a,ix);
    Msg("gamma_p(" & R.fmt(a) & "," & R.fmt(ix) & ")="
       & R.fmt(y) &  "\n");
  END;

  IF NOT Verify(ftn,"gamma_p",R.gamma_p(3.0d0,2.0d0),0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestGamma_p;
(*----------------------*)
PROCEDURE TestGamma_q():BOOLEAN=
CONST
  ftn = Module & "TestGamma_q";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:REAL64;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=R.One-R.gamma_q(a,ix);
    Msg("1-gamma_q(" & R.fmt(a) & "," & R.fmt(ix) & ")="
       & R.fmt(y) &  "\n");
  END;

  IF NOT Verify(ftn,"gamma_q",R.gamma_q(3.0d0,2.0d0),
                            R.One-0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestGamma_q;
(*-------------------------*)
PROCEDURE TestReal64():BOOLEAN=
CONST ftn = Module & "TestReal64";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestLn_gamma();
  NewLine(); EVAL TestBetai();
  NewLine(); EVAL TestGamma_p();
  NewLine(); EVAL TestGamma_q();
  RETURN result;
END TestReal64;
(*=======================*)
BEGIN
END TestReal64.
