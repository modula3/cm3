MODULE TestLapack EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)

IMPORT Fmt;

(*=======================*)
CONST
  Module = "TestLapack.";

(*----------------------*)
PROCEDURE TestLinAlg():BOOLEAN=
CONST
  ftn = Module & "TestLinAlg";
CONST
  id = ARRAY OF CHAR {'e', 's' , 'b', 'p', 'n', 'r', 'm', 'u', 'l', 'o'};

VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR j:=FIRST(id) TO LAST(id) DO
    Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT
         {Fmt.Char(id[j]), Fmt.LongReal(GetMachineParameter(id[j]))}));
  END;

  RETURN result;
END TestLinAlg;
(*-------------------------*)
PROCEDURE TestLapack():BOOLEAN=
<*UNUSED*> CONST ftn = Module & "TestLapack";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestLinAlg();
  RETURN result;
END TestLapack;
(*=======================*)
BEGIN
END TestLapack.
