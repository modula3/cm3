(* See if each TRY has a frame, or if just the function has one. *)

UNSAFE MODULE Main;
FROM IO IMPORT Put, PutInt;

CONST T = Put;
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
  NL();
END PrintStackHeight;

PROCEDURE Try1() =
    PROCEDURE Try1_Try2() = BEGIN
    T(">Try1_Try2"); NL();
    TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END;
    T("<Try1_Try2"); NL();
    END Try1_Try2;
BEGIN
T(">Try1"); NL();
Try1_Try2();
TRY PrintStackHeight() EXCEPT ELSE END;
T("<Try1"); NL();
END Try1;

PROCEDURE Try2() =
    PROCEDURE Try2_Try2() = BEGIN
    T(">Try2_Try2"); NL();
    TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END;
    T("<Try2_Try2"); NL();
    END Try2_Try2;
BEGIN
T(">Try2"); NL();
Try2_Try2();
TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END;
T("<Try2"); NL();
END Try2;

PROCEDURE Try3() =
    PROCEDURE Try3_Try1() = BEGIN
    T(">Try3_Try1"); NL();
    TRY PrintStackHeight() EXCEPT ELSE END;
    T("<Try3_Try1"); NL();
    END Try3_Try1;

    PROCEDURE Try3_Try2() = BEGIN
    T(">Try3_Try2"); NL();
    TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END;
    T("<Try3_Try2"); NL();
    END Try3_Try2;

    PROCEDURE Try3_Try3() = BEGIN
    T(">Try3_Try3"); NL();
    TRY TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END EXCEPT ELSE END;
    T("<Try3_Try3"); NL();
    END Try3_Try3;
BEGIN
T(">Try3"); NL();
Try3_Try1();
Try3_Try2();
Try3_Try3();
TRY TRY TRY PrintStackHeight() EXCEPT ELSE END EXCEPT ELSE END EXCEPT ELSE END;
T("<Try3"); NL();
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
