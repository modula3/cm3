MODULE TestFFT EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for FFT module.

3/23/96  Harry George   Initial version

*)

IMPORT xFFT;
(*=======================*)
CONST
  Module = "TestFFT.";
(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;
END TestABC;
(*----------------------*)
PROCEDURE TestBasic_FFT():BOOLEAN=
CONST
  ftn = Module & "TestBasic_FFT";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  xFFT.Test();
  Msg("if we got here, FFT is ok\n");
  RETURN result;
END TestBasic_FFT;
(*-------------------------*)
PROCEDURE TestFFT():BOOLEAN=
CONST ftn = Module & "TestFFT";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestBasic_FFT();
  RETURN result;
END TestFFT;
(*=======================*)
BEGIN
END TestFFT.
