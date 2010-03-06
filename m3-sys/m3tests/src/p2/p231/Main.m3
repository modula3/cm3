MODULE Main;
IMPORT Long;

(* This valid program as of March 6 2010 fails to compile for NT386 (m3back) *)

VAR a: LONGINT;

PROCEDURE F1() =
BEGIN
  EVAL Long.Insert(a * 0L, a * 0L, 0, 0);
END F1;

BEGIN
  F1();
END Main.
