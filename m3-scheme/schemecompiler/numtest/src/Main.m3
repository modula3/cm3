MODULE Main;
IMPORT SchemeM3, Scheme, Pathname, Csighandler;
IMPORT Debug, Wr, AL;
IMPORT SchemeNavigatorEnvironment, SchemeEnvironment;
IMPORT ParseParams, Stdio;
IMPORT TextRd, SchemeInputPort;

(* Force compiled modules to be linked and registered *)
IMPORT NumtestCompiled; <*NOWARN*>

TYPE
  Interrupter = Scheme.Interrupter OBJECT
  OVERRIDES
    interrupt := Interrupt;
  END;

PROCEDURE Interrupt(<*UNUSED*>i : Interrupter) : BOOLEAN =
  BEGIN
    IF Csighandler.have_signal() = 1 THEN
      Csighandler.clear_signal();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END Interrupt;

VAR
  env : SchemeEnvironment.T := NEW(SchemeNavigatorEnvironment.T).initEmpty();
  evalExpr : TEXT := NIL;
BEGIN
  Csighandler.install_int_handler();

  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-e") THEN
        evalExpr := pp.getNext()
      END;

      pp.skipParsed();
  WITH arr = NEW(REF ARRAY OF Pathname.T, 1 + NUMBER(pp.arg^) - pp.next) DO
    arr[0] := "require";
    FOR i := 1 TO LAST(arr^) DO arr[i] := pp.getNext() END;
    pp.finish();

    TRY
      WITH scm = NEW(SchemeM3.T).init(arr^, globalEnv := env) DO
        IF evalExpr # NIL THEN
          EVAL scm.loadPort(NEW(SchemeInputPort.T).init(
                              NEW(TextRd.T).init(evalExpr)))
        ELSE
          scm.readEvalWriteLoop(NEW(Interrupter))
        END
      END
    EXCEPT
      Scheme.E(err) =>
      Debug.Error("Scheme.E: " & err)
    |
      Wr.Failure (err) =>
      Debug.Error("Wr.Failure: " & AL.Format(err))
    END
  END
    END
  EXCEPT
    ParseParams.Error => Debug.Error("check usage, ParseParams error")
  END;

END Main.
