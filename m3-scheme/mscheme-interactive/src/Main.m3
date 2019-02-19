(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT SchemeM3, Scheme, Params, Pathname, Csighandler;
IMPORT Debug, OSError, Wr, AL;
IMPORT SchemeNavigatorEnvironment, SchemeEnvironment;
IMPORT ParseParams, Stdio;

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
BEGIN 
  Csighandler.install_int_handler();

  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-unsafeenv") THEN
        env := NEW(SchemeEnvironment.Unsafe).initEmpty();
      END;

      pp.skipParsed();
  WITH arr = NEW(REF ARRAY OF Pathname.T, 1 + NUMBER(pp.arg^) - pp.next) DO
    arr[0] := "require";
    FOR i := 1 TO LAST(arr^) DO arr[i] := pp.getNext() END;
    pp.finish();

    TRY
      WITH scm = NEW(SchemeM3.T).init(arr^,globalEnv := env) DO
        scm.readEvalWriteLoop(NEW(Interrupter))
      END
    EXCEPT
      Scheme.E(err) =>
      Debug.Error("Couldn't initialize Scheme interpreter from files : " & err)
    |
      OSError.E (err) =>
      Debug.Error("Main: Couldn't initialize Scheme from files : OSError.E : "&
        AL.Format(err))
    |
      Wr.Failure (err) =>
      Debug.Error("Main: Couldn't initialize Scheme from files : Wr.Failure : "&
        AL.Format(err))
    END
  END
    END
  EXCEPT
    ParseParams.Error => Debug.Error("check usage, ParseParams error")
  END;

END Main.
