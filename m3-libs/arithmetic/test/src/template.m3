MODULE tXYZ EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)


(*=======================*)
CONST
  Module = "tXYZ.";
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
(*-------------------------*)
PROCEDURE test_XYZ():BOOLEAN=
CONST ftn = Module & "test_XYZ";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_ABC();
  RETURN result;
END test_XYZ;
(*=======================*)
BEGIN
END tXYZ.
