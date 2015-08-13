UNSAFE MODULE Main;

IMPORT Compiler;
FROM Fmt IMPORT Int;
FROM IO IMPORT Put, PutInt;

EXCEPTION E;

VAR top_of_stack:ADDRESS;

PROCEDURE Line(): TEXT = BEGIN RETURN Int(Compiler.ThisLine()); END Line;

PROCEDURE GetStack(): ADDRESS =
VAR a := ADR(a);
BEGIN
  (* RETURN ADR(a); warning about returning the address of local *)
  RETURN a;
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

PROCEDURE NL() = BEGIN Put("\n"); END NL;

PROCEDURE F0() =
CONST Function = "F0 ";
BEGIN
  Put(Function); NL();
  PrintStackHeight();
END F0;

PROCEDURE F1() RAISES ANY =
CONST Function = "F1 ";
BEGIN
  Put(Function); NL();
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Function & Line()); NL();
    RAISE E;
  FINALLY
    Put(Function & Line()); NL();
  END;
END F1;

PROCEDURE F2() RAISES ANY =
CONST Function = "F2 ";
BEGIN
  Put(Function); NL();
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Function & Line()); NL();
    PrintStackHeight();
    TRY
      PrintStackHeight();
      Put(Function & Line()); NL();
      RAISE E;
    FINALLY
      Put(Function & Line()); NL();
    END;
  FINALLY
    Put(Function & Line()); NL();
  END;
END F2;

PROCEDURE F3() RAISES ANY =
VAR Function := "F3 ";
    i: CARDINAL;
BEGIN
  Put(Int(i) & " " & Function & Line()); NL(); INC(i);
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Int(i) & " " & Function & Line()); NL(); INC(i);
    PrintStackHeight();
    TRY
      PrintStackHeight();
      Put(Int(i) & " " & Function & Line()); NL(); INC(i);
      TRY
        PrintStackHeight();
        Put(Int(i) & " " & Function & Line()); NL(); INC(i);
        RAISE E;
      FINALLY
        Function := "finally F3 ";
        Put(Int(i) & " " & Function & Line()); NL(); INC(i);
      END;
    FINALLY
      Put(Int(i) & " " & Function & Line()); NL(); INC(i);
    END;
  FINALLY
    Put(Int(i) & " " & Function & Line()); NL(); INC(i);
  END;
END F3;

EXCEPTION E1;
EXCEPTION E2;
EXCEPTION E3;

PROCEDURE F4() =
CONST Function = "F4 ";
BEGIN
  Put(Function & Line()); NL();
  PrintStackHeight();
  TRY
    Put(Function & Line()); NL();
    PrintStackHeight();
    TRY
      Put(Function & Line()); NL();
      PrintStackHeight();
      TRY
        Put(Function & Line()); NL();
        PrintStackHeight();
        RAISE E1;
      EXCEPT ELSE
        RAISE E2;
      END;
    EXCEPT ELSE
      RAISE E3;
    END;
  EXCEPT ELSE
  END;
END F4;

PROCEDURE F5() =
CONST Function = "F5 ";
BEGIN
  FOR i := 1 TO 10 DO
    TRY
      Put(Function & Line()); NL();
      PrintStackHeight();
      RAISE E1;
    EXCEPT ELSE
    END
  END;
END F5;

PROCEDURE F6() =
CONST Function = "F6 ";
BEGIN
  FOR i := 1 TO 10 DO
    TRY
      Put(Function & Line()); NL();
      PrintStackHeight();
    FINALLY
    END
  END;
END F6;

PROCEDURE Main() =
BEGIN
  top_of_stack := GetStack();
  F0();
  TRY F1(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F2(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F3(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F4(); EXCEPT END;
  TRY F5(); EXCEPT END;
  TRY F6(); EXCEPT END;
END Main;

PROCEDURE Finally () =
BEGIN
  (* same thing but in FINALLY, and nested FINALLY *)
  (* NOTE: This testing is haphazard as I don't
     understand exception handling enough to aim for coverage. *)
  TRY
    top_of_stack := GetStack();
    F0();
  FINALLY
    TRY F1(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
    TRY F2(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
    TRY F3(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
    TRY F4(); EXCEPT END;
    TRY F5(); EXCEPT END;
    TRY F6(); EXCEPT END;
  END;
END Finally;

PROCEDURE NestedFinally() =
BEGIN
  (* same thing but in FINALLY, and nested FINALLY *)
  (* NOTE: This testing is haphazard as I don't
     understand exception handling enough to aim for coverage. *)
  TRY
    top_of_stack := GetStack();
    F0();

  FINALLY
    TRY TRY F1(); FINALLY F0(); END; EXCEPT ELSE Put("exception " & Line()); NL(); END;
    TRY TRY F1(); FINALLY F0(); END; EXCEPT ELSE Put("exception " & Line()); NL(); END;
    TRY TRY F1(); FINALLY F0(); END; EXCEPT ELSE Put("exception " & Line()); NL(); END; TRY TRY F1(); FINALLY F0(); END; EXCEPT ELSE Put("exception " & Line()); NL(); END;
    
    TRY
      TRY
        F2();
      EXCEPT
      ELSE
        Put("exception " & Line()); NL();
      END;
    FINALLY
      F0();
    END;    
  END;

  TRY top_of_stack := GetStack(); TRY F0();
  FINALLY TRY F0(); FINALLY F0(); END; END; FINALLY TRY F0(); FINALLY F0(); END; END;

END NestedFinally;

BEGIN
  Main();

  (* same thing but in Module main *)

  top_of_stack := GetStack();
  F0();
  TRY F1(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F2(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F3(); EXCEPT ELSE Put("exception " & Line()); NL(); END;
  TRY F4(); EXCEPT END;
  TRY F5(); EXCEPT END;
  TRY F6(); EXCEPT END;

  Finally();
  NestedFinally();

END Main.
