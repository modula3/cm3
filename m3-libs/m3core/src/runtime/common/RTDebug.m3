(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE RTDebug EXPORTS RTDebug, RTHooks;

IMPORT RT0, RTIO, Compiler;

CONST
  EOL = ARRAY Compiler.OS OF TEXT { "\n", "\r\n" }[ Compiler.ThisOS ];

VAR
  handler: Handler := NIL;
  mu := NEW (MUTEX);

PROCEDURE RegisterHandler (p: Handler) =
  BEGIN
    handler := p;
  END RegisterHandler;

(* RTHooks.DebugMsg *)
PROCEDURE DebugMsg (module: ADDRESS(*RT0.ModulePtr*);  line: INTEGER;
                    READONLY msg: ARRAY OF TEXT) RAISES ANY =
  VAR p := handler;
  BEGIN
    IF (p = NIL) THEN p := DefaultMsg; END;
    LOCK mu DO
      p (module, line, msg);
    END;
  END DebugMsg;

PROCEDURE DefaultMsg (m: RT0.ModulePtr;  line: INTEGER;
                      READONLY msg: ARRAY OF TEXT) =
  BEGIN
    PrintHeader (m, line);
    FOR i := FIRST (msg) TO LAST (msg) DO
      IF msg[i] # NIL THEN RTIO.PutText (msg[i]); END;
    END;
    RTIO.PutText (EOL);
    RTIO.Flush ();
  END DefaultMsg;

PROCEDURE PrintHeader (m: RT0.ModulePtr;  line: INTEGER) =
  BEGIN
    IF (m # NIL) AND (m.file # NIL) THEN
      RTIO.PutString (m.file);
      IF (line # 0) THEN
        RTIO.PutText (", line ");
        RTIO.PutInt (line);
      END;
      RTIO.PutText (": ");
    ELSIF (line # 0) THEN
      RTIO.PutText ("line ");
      RTIO.PutInt (line);
      RTIO.PutText (": ");
    END;
  END PrintHeader;

BEGIN
END RTDebug.
