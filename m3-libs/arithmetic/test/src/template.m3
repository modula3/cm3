MODULE TestXYZ EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)


(*=======================*)
CONST
  Module = "TestXYZ.";
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
(*-------------------------*)
PROCEDURE TestXYZ():BOOLEAN=
CONST ftn = Module & "TestXYZ";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestABC();
  RETURN result;
END TestXYZ;
(*=======================*)
BEGIN
END TestXYZ.
