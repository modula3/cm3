MODULE TestXYZ EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Tests for XYZ module. *)



CONST Module = "TestXYZ.";

PROCEDURE TestABC (): BOOLEAN =
  CONST ftn = Module & "TestABC";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");

    RETURN result;
  END TestABC;

PROCEDURE TestXYZ (): BOOLEAN =
  CONST ftn = Module & "TestXYZ";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestABC();
    RETURN result;
  END TestXYZ;

BEGIN
END TestXYZ.
