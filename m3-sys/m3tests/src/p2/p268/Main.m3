(* Grisu vs. C backend *)

MODULE Main;
IMPORT Fmt;

PROCEDURE Test() =
BEGIN
  EVAL Fmt.LongReal(0.047218084335327148D0);
END Test;

BEGIN
 Test();
END Main.
