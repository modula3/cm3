(* Moved from p268, which was duplicated by an unrelated test in the
   packedVars branch. *) 

(* Bug is not yet understood/fixed, but it works for gcc backend at least
  (and integrated?)
  This is derived from m3-comm/sharedobjgen.
*) 

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

