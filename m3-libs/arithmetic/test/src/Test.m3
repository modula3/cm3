MODULE Test EXPORTS Test,Main;
(*Copyright (c) 1996,
Abstract: Test driver for m3na library.

12/13/95  Harry George   Initial version
1/27/96   Harry George   converted to m3na format
*)

IMPORT Stdio,Thread,Wr,LongRealBasic AS R;
FROM LongRealTrans IMPORT Eps;

<*FATAL Thread.Alerted, Wr.Failure*>

(*=======================*)
<*UNUSED*> CONST Module = "Test.";
(*-----------------*)
PROCEDURE Msg(str:TEXT) =
BEGIN
  Wr.PutText(Stdio.stdout,str);
  Wr.Flush(Stdio.stdout);
END Msg;
(*-----------------*)
PROCEDURE Debug(level:[0..3]; ftn,str:TEXT) =
BEGIN
  IF verbosity >= level THEN
    (*Debugging levels*)
    Wr.PutText(Stdio.stdout,ftn & ":" & str);
    Wr.Flush(Stdio.stdout);
  END;
END Debug;
(*---------------------*)
PROCEDURE Verify(ftn,str:TEXT;
                 expected,found:R.T;
                 eps:R.T:=Eps
                 ):BOOLEAN=
BEGIN
  IF ABS(expected-found)<eps THEN
    Msg(ftn & ":" & str & ":passed\n");
    RETURN TRUE;
  ELSE
    Msg(ftn & ":" & str & ":failed\n");
    RETURN FALSE;
  END;
END Verify;
(*---------------------*)
PROCEDURE NewLine()=
BEGIN
  Msg("\n");
END NewLine;
(*=====================================*)

(*=======================*)
BEGIN
(*
  NewLine(); EVAL TestWordEx();
  NewLine(); EVAL TestBits();
  NewLine(); EVAL TestInteger();
  NewLine(); EVAL TestReal64();
  NewLine(); EVAL TestComplex();
  NewLine(); EVAL TestPolynomial();
*)
  NewLine(); EVAL TestRoot();
(*
  NewLine(); EVAL TestInterpolation();
  NewLine(); EVAL TestVector();
  NewLine(); EVAL TestMatrix();
  NewLine(); EVAL TestSLE();
  NewLine(); EVAL TestRandom();
  NewLine(); EVAL TestFFT();
  NewLine(); EVAL TestBigInteger();
  NewLine(); EVAL TestGCD();
  NewLine(); EVAL TestEigenSystem();
  NewLine(); EVAL TestChebyshev();
  NewLine(); EVAL TestUnit();
  NewLine(); EVAL TestLapack();
  NewLine(); EVAL TestTex();
  NewLine(); EVAL TestPLPlot();
  NewLine(); EVAL TestFunctional();
*)
END Test.
