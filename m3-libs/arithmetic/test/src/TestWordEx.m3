MODULE tWordEx EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for WordEx module.

3/23/96 Harry George   Initial version

*)
IMPORT xWordEx;

(*=======================*)
CONST
  Module = "tWordEx.";
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
PROCEDURE test_wordex():BOOLEAN=
CONST
  ftn = Module & "test_wordex";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  xWordEx.Test();
  msg("ok\n");
  RETURN result;   
END test_wordex;
(*-------------------------*)
PROCEDURE test_WordEx():BOOLEAN=
CONST ftn = Module & "test_WordEx";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_wordex();
  RETURN result;
END test_WordEx;
(*=======================*)
BEGIN
END tWordEx.
