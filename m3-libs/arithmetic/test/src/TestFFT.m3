MODULE TestFFT EXPORTS Test;
(*Copyright (c) 1996, m3na project Abstract: Tests for FFT module.

   3/23/96 Harry George Initial version

   *)

IMPORT LongRealFourierTransform AS FT;
(*=======================*)
CONST Module = "TestFFT.";
(*----------------------*)
PROCEDURE TestBasicFFT (): BOOLEAN =
  CONST ftn = Module & "TestBasicFFT";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    FT.Test();
    Msg("if we got here, FFT is ok\n");
    RETURN result;
  END TestBasicFFT;
(*-------------------------*)
PROCEDURE TestFFT (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestFFT";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestBasicFFT();
    RETURN result;
  END TestFFT;
(*=======================*)
BEGIN
END TestFFT.
