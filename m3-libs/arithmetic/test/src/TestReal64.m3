MODULE TestReal64 EXPORTS Test;
(*Copyright (c) 1995,1996 Harry George
Abstract: Test driver for numerical analysis

12/24/95  Harry George   Initial version
2/17/96   Harry George   Rebuilt into Real64

*)

IMPORT SpecialFunction AS SF,
       LongRealBasic   AS R,
       LongRealFmtLex  AS RF;
FROM SF IMPORT T;
(*=======================*)
CONST
  Module = "TestReal64.";
VAR
(*=======================*)

(*----------------------*)
PROCEDURE TestLnGamma():BOOLEAN=
CONST
  ftn = Module & "TestLnGamma";
VAR
  result:=TRUE;
  ix,g:T;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 10 DO
    ix:=ix+10.0d0;
    g:=SF.LnGamma(ix);
    Msg("LnGamma(" & RF.Fmt(ix) & ")=" & RF.Fmt(g) & "\n");
  END;

  IF NOT Verify(ftn,"LnGamma",SF.LnGamma(10.0d0),
                             12.80182748008147D0,1.0d4) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestLnGamma;
(*----------------------*)
PROCEDURE TestBetaI():BOOLEAN=
CONST
  ftn = Module & "TestBetaI";
VAR
  result:=TRUE;
  ix,b:T;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+0.1d0;
    b:=SF.BetaI(1.0d0,3.0d0,ix);
    Msg("BetaI(1.0d0,3.0d0," & RF.Fmt(ix) & ")="
       &RF.Fmt(b) &  "\n");
  END;

  IF NOT Verify(ftn,"BetaI",SF.BetaI(1.0d0,3.0d0,0.2d0),0.488d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestBetaI;
(*----------------------*)
PROCEDURE TestGammaP():BOOLEAN=
CONST
  ftn = Module & "TestGammaP";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:T;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=SF.GammaP(a,ix);
    Msg("GammaP(" & RF.Fmt(a) & "," & RF.Fmt(ix) & ")="
       & RF.Fmt(y) &  "\n");
  END;

  IF NOT Verify(ftn,"GammaP",SF.GammaP(3.0d0,2.0d0),0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestGammaP;
(*----------------------*)
PROCEDURE TestGammaQ():BOOLEAN=
CONST
  ftn = Module & "TestGammaQ";
VAR
  result:=TRUE;
  a:=3.0d0;
  ix,y:T;
BEGIN
  Debug(1,ftn,"begin\n");
  ix:=R.Zero;
  FOR i:=1 TO 9 DO
    ix:=ix+R.One;
    y:=R.One-SF.GammaQ(a,ix);
    Msg("1-GammaQ(" & RF.Fmt(a) & "," & RF.Fmt(ix) & ")="
       & RF.Fmt(y) &  "\n");
  END;

  IF NOT Verify(ftn,"GammaQ",SF.GammaQ(3.0d0,2.0d0),
                            R.One-0.323d0,0.01d0) THEN
    result:=FALSE;
  END;
  RETURN result;
END TestGammaQ;
(*-------------------------*)
PROCEDURE TestReal64():BOOLEAN=
CONST ftn = Module & "TestReal64";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestLnGamma();
  NewLine(); EVAL TestBetaI();
  NewLine(); EVAL TestGammaP();
  NewLine(); EVAL TestGammaQ();
  RETURN result;
END TestReal64;
(*=======================*)
BEGIN
END TestReal64.
