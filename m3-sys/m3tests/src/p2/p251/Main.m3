UNSAFE MODULE Main;

IMPORT IO, Compiler, Fmt;
FROM Fmt IMPORT Int;
FROM IO IMPORT Put, PutInt;
CONST Line = Compiler.ThisLine;

EXCEPTION E;

VAR top_of_stack:ADDRESS;

PROCEDURE GetStack(): ADDRESS =
VAR a:INTEGER;
BEGIN
  RETURN ADR(a);
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

PROCEDURE F1() RAISES ANY =
CONST Function = "F1 ";
BEGIN
  Put(Function); NL();
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Function & Int(Line())); NL();
    RAISE E;
  FINALLY
    Put(Function & Int(Line())); NL();
  END;
END F1;

PROCEDURE F2() RAISES ANY =
CONST Function = "F2 ";
BEGIN
  Put(Function); NL();
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Function & Int(Line())); NL();
    PrintStackHeight();
    TRY
      PrintStackHeight();
      Put(Function & Int(Line())); NL();
      RAISE E;
    FINALLY
      Put(Function & Int(Line())); NL();
    END;
  FINALLY
    Put(Function & Int(Line())); NL();
  END;
END F2;


PROCEDURE F3() RAISES ANY =
CONST Function = "F3 ";
BEGIN
  Put(Function & Int(Line())); NL();
  PrintStackHeight();
  TRY
    PrintStackHeight();
    Put(Function & Int(Line())); NL();
    PrintStackHeight();
    TRY
      PrintStackHeight();
      Put(Function & Int(Line())); NL();
      TRY
        PrintStackHeight();
        Put(Function & Int(Line())); NL();
        RAISE E;
      FINALLY
        Put(Function & Int(Line())); NL();
      END;
    FINALLY
      Put(Function & Int(Line())); NL();
    END;
  FINALLY
    Put(Function & Int(Line())); NL();
  END;
END F3;

EXCEPTION E1;
EXCEPTION E2;
EXCEPTION E3;

PROCEDURE F4() =
CONST Function = "F4 ";
BEGIN
  Put(Function & Int(Line())); NL();
  TRY
    Put(Function & Int(Line())); NL();
    TRY
      Put(Function & Int(Line())); NL();
      TRY
        Put(Function & Int(Line())); NL();
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
      Put(Function & Int(Line())); NL();
      PrintStackHeight();
      RAISE E1;
    EXCEPT ELSE
    END
  END;
END F5;

PROCEDURE Main() =
BEGIN
  top_of_stack := GetStack();
  TRY F1(); EXCEPT ELSE Put("exception " & Int(Line())); NL(); END;
  TRY F2(); EXCEPT ELSE Put("exception " & Int(Line())); NL(); END;
  TRY F3(); EXCEPT ELSE Put("exception " & Int(Line())); NL(); END;
  TRY F4(); EXCEPT END;
  TRY F5(); EXCEPT END;
END Main;

BEGIN
  Main();
END Main.
