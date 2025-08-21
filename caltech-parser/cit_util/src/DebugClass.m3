(* $Id$ *)
MODULE DebugClass;

(* initialize level from DEBUGLEVEL before doing anything else *)

IMPORT Env, Scan, Lex, FloatMode, Debug, Fmt;
IMPORT IO;
IMPORT DebugStreamList, DebugStream;
FROM Stdio IMPORT stderr;

VAR init := FALSE;

PROCEDURE DoInit() =
BEGIN
  IF init = TRUE THEN RETURN ELSE init := TRUE END;

  streams := DebugStreamList.List1(DebugStream.T { stderr });

  mu := NEW(MUTEX);
  VAR
    debugStr := Env.Get("DEBUGLEVEL");
  BEGIN
    TRY
      IF debugStr # NIL THEN level := Scan.Int(debugStr) END;
      Debug.Out("level set to " & Fmt.Int(level))
    EXCEPT
      Lex.Error, FloatMode.Trap =>
        Debug.Error("DEBUGLEVEL set to nonsense! \"" & debugStr & "\"",TRUE)
    END
  END
END DoInit;

BEGIN DoInit() END DebugClass.


