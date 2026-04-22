(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeSamplingProfiler;
IMPORT Thread, TextIntTbl;

VAR
  mu := NEW(MUTEX);
  tab : TextIntTbl.T := NEW(TextIntTbl.Default).init();
  total : INTEGER := 0;
  running := FALSE;
  thread : Thread.T := NIL;

TYPE
  SamplerClosure = Thread.Closure OBJECT
    intervalSecs : LONGREAL;
  OVERRIDES
    apply := SamplerApply;
  END;

PROCEDURE SamplerApply(cl : SamplerClosure) : REFANY =
  BEGIN
    LOOP
      Thread.Pause(cl.intervalSecs);
      IF NOT enabled THEN EXIT END;
      VAR name : TEXT := currentProcName; BEGIN
        IF name # NIL THEN
          LOCK mu DO
            VAR count := 0; BEGIN
              EVAL tab.get(name, count);
              EVAL tab.put(name, count + 1);
              INC(total)
            END
          END
        END
      END
    END;
    RETURN NIL
  END SamplerApply;

PROCEDURE Start(intervalMS : CARDINAL := 1) =
  VAR ms := MAX(intervalMS, 1);
  BEGIN
    LOCK mu DO
      IF running THEN RETURN END;
      enabled := TRUE;
      running := TRUE;
      thread := Thread.Fork(
                    NEW(SamplerClosure,
                        intervalSecs := FLOAT(ms, LONGREAL) / 1000.0d0))
    END
  END Start;

PROCEDURE Stop() =
  VAR t : Thread.T;
  BEGIN
    LOCK mu DO
      enabled := FALSE;
      t := thread;
    END;
    IF t # NIL THEN
      EVAL Thread.Join(t);
      LOCK mu DO
        thread := NIL;
        running := FALSE
      END
    END
  END Stop;

PROCEDURE Reset() =
  BEGIN
    LOCK mu DO
      tab := NEW(TextIntTbl.Default).init();
      total := 0
    END
  END Reset;

PROCEDURE Results() : TextIntTbl.T =
  BEGIN
    LOCK mu DO RETURN tab END
  END Results;

PROCEDURE Total() : INTEGER =
  BEGIN
    LOCK mu DO RETURN total END
  END Total;

BEGIN END SchemeSamplingProfiler.
