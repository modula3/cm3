MODULE test EXPORTS test,Main;
(*Copyright (c) 1996, 
Abstract: Test driver for m3na library.

12/13/95  Harry George   Initial version
1/27/96   Harry George   converted to m3na format
*)

IMPORT Stdio,Wr,Fmt;
FROM xReal64 IMPORT REAL64,EPS;
(*=======================*)
<*UNUSED*> CONST Module = "Test.";
(*-----------------*)
PROCEDURE msg(str:TEXT) =
BEGIN
  Wr.PutText(Stdio.stdout,str);
  Wr.Flush(Stdio.stdout);
END msg;
(*-----------------*)
PROCEDURE debug(level:[0..3]; ftn,str:TEXT) =
BEGIN
  IF verbosity >= level THEN
    (*debugging levels*)
    Wr.PutText(Stdio.stdout,ftn & ":" & str);
    Wr.Flush(Stdio.stdout);
  END;
END debug;
(*---------------------*)
PROCEDURE verify(ftn,str:TEXT;
                 expected,found:REAL64;
                 eps:REAL64:=EPS 
                 ):BOOLEAN=
BEGIN
  IF ABS(expected-found)<eps THEN
    msg(ftn & ":" & str & ":passed\n");
    RETURN TRUE;
  ELSE
    msg(ftn & ":" & str & ":failed\n");
    RETURN FALSE;
  END;
END verify;  
(*---------------------*)
PROCEDURE newline()=
BEGIN
  msg("\n");
END newline;
(*=====================================*)

(*=======================*)
BEGIN
  newline(); EVAL test_WordEx();
  (*newline(); EVAL test_Bits();*)
  (*newline(); EVAL test_Integer();*)
  (*newline(); EVAL test_Real64();*)
  (*newline(); EVAL test_Cmplx();*)
  (*newline(); EVAL test_Vect();*)
  (*newline(); EVAL test_Mat();*)
  (*newline(); EVAL test_Poly();*)
  (*newline(); EVAL test_Root();*)
  (*newline(); EVAL test_Interp();*)
  (*newline(); EVAL test_SLE();*)
  (*newline(); EVAL test_Rand();*)
  (*newline(); EVAL test_FFT();*)
  newline(); EVAL test_BigInteger();
END test.
