(* With -OverflowChk (set in this test's m3makefile), integer overflow in
   a constant expression is an error, not a warning: the run-time result
   would abort, so the constant has no value. *)

MODULE Main;

CONST
  A = LAST (INTEGER) + 1;
  B = FIRST (INTEGER) - 1;
  C = LAST (LONGINT) * 2L;
  D = - FIRST (INTEGER);
  E = LAST (INTEGER) - 1;  (* fine *)

BEGIN
  EVAL A;  EVAL B;  EVAL C;  EVAL D;  EVAL E;
END Main.
