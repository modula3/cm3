MODULE Main;
IMPORT IO, Compiler;
FROM Compiler IMPORT ThisLine;
FROM IO IMPORT PutInt;

PROCEDURE NL() = BEGIN IO.Put("\n"); END NL;

PROCEDURE F1() =
BEGIN
  TRY
    PutInt(ThisLine()); NL();
  FINALLY
    PutInt(ThisLine()); NL();
  END;
END F1;

PROCEDURE F2() =
BEGIN
  TRY
    PutInt(ThisLine()); NL();
    TRY
      PutInt(ThisLine()); NL();
      FINALLY
        PutInt(ThisLine()); NL();
      END;
  FINALLY
    PutInt(ThisLine()); NL();
  END;
END F2;

BEGIN
  TRY F1(); EXCEPT END;
  TRY F2(); EXCEPT END;
END Main.
