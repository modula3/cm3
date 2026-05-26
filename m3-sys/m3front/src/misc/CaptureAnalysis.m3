(* Copyright (C) 2026, the CM3 contributors.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE CaptureAnalysis;

IMPORT Variable;

CONST InitSize = 8;

REVEAL
  T = BRANDED "CaptureAnalysis.T" REF RECORD
    n   : INTEGER        := 0;
    cap : REF ARRAY OF Capture;
  END;

PROCEDURE New (): T =
  BEGIN
    RETURN NEW (T, n := 0, cap := NEW (REF ARRAY OF Capture, InitSize))
  END New;

PROCEDURE Note (ca: T;  v: Variable.T;  written: BOOLEAN) =
  BEGIN
    FOR i := 0 TO ca.n - 1 DO
      IF ca.cap[i].var = v THEN
        IF written THEN ca.cap[i].written := TRUE END;
        RETURN
      END
    END;
    IF ca.n = NUMBER (ca.cap^) THEN
      VAR new := NEW (REF ARRAY OF Capture, ca.n * 2); BEGIN
        SUBARRAY (new^, 0, ca.n) := SUBARRAY (ca.cap^, 0, ca.n);
        ca.cap := new
      END
    END;
    ca.cap[ca.n] := Capture{var := v, written := written};
    INC (ca.n)
  END Note;

PROCEDURE GetCaptures (ca: T): REF ARRAY OF Capture =
  VAR result := NEW (REF ARRAY OF Capture, ca.n); BEGIN
    SUBARRAY (result^, 0, ca.n) := SUBARRAY (ca.cap^, 0, ca.n);
    RETURN result
  END GetCaptures;

PROCEDURE Remove (ca: T;  v: Variable.T) =
  BEGIN
    FOR i := 0 TO ca.n - 1 DO
      IF ca.cap[i].var = v THEN
        FOR j := i TO ca.n - 2 DO ca.cap[j] := ca.cap[j+1] END;
        DEC (ca.n);
        RETURN
      END
    END
  END Remove;

BEGIN
END CaptureAnalysis.
