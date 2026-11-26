(* <*DEBUG*> pragmas, unconditional, with a non-constant condition, and with
   FALSE: each prints the module, the line and its messages (RTHooks.DebugMsg)
   when its condition holds. *)

MODULE Main;
IMPORT Fmt;
VAR n := 3;
PROCEDURE P (k: INTEGER) =
  BEGIN
    <*DEBUG "unconditional ", Fmt.Int (k) *>
    <*DEBUG k > 2 WITH "k is big: ", Fmt.Int (k) *>
    <*DEBUG FALSE WITH "never" *>
  END P;
BEGIN
  P (1);  P (n);
END Main.
