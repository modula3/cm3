(* This program files to link with the
 * gcc backend if compiling "unit at a time"
Undefined symbols:
  "_Main__F1__NestedUnused1.496", referenced from:
      _L_1 in Main.mo
 *)
MODULE Main;

PROCEDURE F1() =
  PROCEDURE NestedUnused1() = BEGIN END NestedUnused1;
<*NOWARN*>CONST a = F1;
BEGIN
END F1;

BEGIN
END Main.
