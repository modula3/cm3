MODULE TestWordEx EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for WordEx module.

3/23/96 Harry George   Initial version

*)
IMPORT xWordEx;

(*=======================*)
CONST
  Module = "TestWordEx.";
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
PROCEDURE TestWordex():BOOLEAN=
CONST
  ftn = Module & "TestWordex";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  xWordEx.Test();
  Msg("ok\n");
  RETURN result;
END TestWordex;
(*-------------------------*)
PROCEDURE TestWordEx():BOOLEAN=
CONST ftn = Module & "TestWordEx";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestWordex();
  RETURN result;
END TestWordEx;
(*=======================*)
BEGIN
END TestWordEx.
