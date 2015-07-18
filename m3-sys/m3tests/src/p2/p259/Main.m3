(* See if each TRY has a frame, or if just the function has one. *)

UNSAFE MODULE Main;
FROM IO IMPORT Put, PutInt;

PROCEDURE NL() = BEGIN Put("\n"); END NL;

VAR top_of_stack:ADDRESS;

PROCEDURE GetStack(): ADDRESS =
VAR b := ADR(b);
BEGIN
  (* RETURN ADR(a); warning about returning the address of local *)
  RETURN b;
END GetStack;

PROCEDURE GetStackHeight(): INTEGER =
VAR b := GetStack();
BEGIN
  IF b > top_of_stack THEN
    RETURN b - top_of_stack;
  END;
  RETURN top_of_stack - b;
END GetStackHeight;

PROCEDURE PrintStackHeight() =
BEGIN
  Put("stack_height:");
  PutInt(GetStackHeight());
  Put(" ");
END PrintStackHeight;

PROCEDURE Try1() = BEGIN
TRY PrintStackHeight() EXCEPT ELSE END
END Try1;

PROCEDURE Try2() = BEGIN
TRY TRY PrintStackHeight()
EXCEPT ELSE END
EXCEPT ELSE END
END Try2;

PROCEDURE Try3() = BEGIN
TRY TRY TRY PrintStackHeight()
EXCEPT ELSE END
EXCEPT ELSE END
EXCEPT ELSE END
END Try3;

BEGIN
  top_of_stack := GetStack();
  Try1(); NL();
  Try2(); NL();
  Try3(); NL();
  Try1(); NL();
  Try2(); NL();
  Try3(); NL();
END Main.
