MODULE TestWordEx EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Tests for WordEx module.

   3/23/96 Harry George Initial version *)
IMPORT WordEx;


CONST Module = "TestWordEx.";

PROCEDURE TestWordex (): BOOLEAN =
  CONST ftn = Module & "TestWordex";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    WordEx.Test();
    Msg("ok\n");
    RETURN result;
  END TestWordex;

PROCEDURE TestWordEx (): BOOLEAN =
  <* UNUSED *>
  CONST
    ftn = Module & "TestWordEx";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestWordex();
    RETURN result;
  END TestWordEx;

BEGIN
END TestWordEx.
