(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE RTError;

IMPORT RTHeapRep, RTProcedure, RTProcedureSRC, RTProcess, RTIO;

PROCEDURE Msg (file            : TEXT;
                      line            : INTEGER;
                      msgA, msgB, msgC: TEXT      := NIL) =
  BEGIN
    StartError (msgA, msgB, msgC);
    IF (file # NIL) THEN
      RTIO.PutText ("\n***    file \"");
      RTIO.PutText (file);
      RTIO.PutText ("\"");
      IF (line # 0) THEN
        RTIO.PutText (", line ");
        RTIO.PutInt  (line);
      END;
    END;
    EndError (crash := TRUE);
  END Msg;

PROCEDURE MsgS (file            : ADDRESS;
                       line            : INTEGER;
                       msgA, msgB, msgC: TEXT      := NIL) =
  BEGIN
    StartError (msgA, msgB, msgC);
    IF (file # NIL) THEN
      RTIO.PutText   ("\n***    file \"");
      RTIO.PutString (file);
      RTIO.PutText   ("\"");
      IF (line # 0) THEN
        RTIO.PutText (", line ");
        RTIO.PutInt  (line);
      END;
    END;
    EndError (crash := TRUE);
  END MsgS;

PROCEDURE MsgI (msg: TEXT := NIL;  i: INTEGER) =
  BEGIN
    StartError (msg);
    RTIO.PutInt (i);
    EndError (crash := TRUE);
  END MsgI;

PROCEDURE MsgPC (pc: INTEGER; msgA, msgB, msgC: TEXT := NIL) =
  BEGIN
    ErrorPC (pc, msgA, msgB, msgC, crash := TRUE);
  END MsgPC;

PROCEDURE ReportPC (pc: INTEGER; msgA, msgB, msgC: TEXT := NIL) =
  BEGIN
    ErrorPC (pc, msgA, msgB, msgC, crash := FALSE);
  END ReportPC;

PROCEDURE ErrorPC (pc: INTEGER; msgA, msgB, msgC: TEXT;  crash: BOOLEAN) =
  VAR
    proc   : RTProcedure.Proc;
    file   : RTProcedureSRC.Name;
    name   : RTProcedureSRC.Name;
    offset : INTEGER;
  BEGIN
    StartError (msgA, msgB, msgC);
    IF RTHeapRep.Crash () THEN
      IF (pc # 0) THEN
        RTIO.PutText ("\n***    pc = ");
        RTIO.PutHex  (pc);
        RTProcedureSRC.FromPC (LOOPHOLE(pc, ADDRESS), proc, file, name);
        offset := pc - LOOPHOLE (proc, INTEGER);
        IF (0 <= offset) AND (offset < 2048) THEN
          IF (name # NIL) THEN
            RTIO.PutText   (" = ");
            RTIO.PutString (name);
            IF (offset # 0) THEN
              RTIO.PutText (" + ");
              RTIO.PutHex  (offset);
            END;
          END;
          IF (file # NIL) THEN
            RTIO.PutText (" in ");
            RTIO.PutString (file);
          END;
        END;
      END;
    END;
    EndError (crash);
  END ErrorPC;

PROCEDURE StartError (msgA, msgB, msgC: TEXT := NIL) =
  BEGIN
    RTIO.PutText ("\n\n***\n*** runtime error:\n***    ");
    IF (msgA # NIL) THEN RTIO.PutText (msgA) END;
    IF (msgB # NIL) THEN RTIO.PutText (msgB) END;
    IF (msgC # NIL) THEN RTIO.PutText (msgC) END;
    RTIO.Flush ();
  END StartError;

PROCEDURE EndError (crash: BOOLEAN) =
  BEGIN
    RTIO.PutText ("\n***\n\n");
    RTIO.Flush ();
    IF crash THEN RTProcess.Crash (NIL); END;
  END EndError;

BEGIN
END RTError.
