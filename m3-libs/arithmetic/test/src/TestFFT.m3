MODULE tFFT EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for FFT module.

3/23/96  Harry George   Initial version

*)

IMPORT xFFT;
(*=======================*)
CONST
  Module = "tFFT.";
(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
(*----------------------*)
PROCEDURE test_basic_FFT():BOOLEAN=
CONST
  ftn = Module & "test_basic_FFT";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  xFFT.Test();
  msg("if we got here, FFT is ok\n");
  RETURN result;   
END test_basic_FFT;
(*-------------------------*)
PROCEDURE test_FFT():BOOLEAN=
CONST ftn = Module & "test_FFT";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_basic_FFT();
  RETURN result;
END test_FFT;
(*=======================*)
BEGIN
END tFFT.
