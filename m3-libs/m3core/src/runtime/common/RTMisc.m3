(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:27:05 PST 1994 by kalsow     *)
(*      modified on Tue May 11 16:49:30 PDT 1993 by mjordan    *)
(*      modified on Tue Apr 20 14:44:12 PDT 1993 by mcjones    *)
(*      modified on Sun Feb 21 14:59:10 PST 1993 by jdd        *)
(*      modified on Sat Jun 27 22:22:30 PDT 1992 by muller     *)

UNSAFE MODULE RTMisc;

IMPORT Cstring, RTHeapRep, RTProcedure, RTProcedureSRC, RTProcess, RTIO;

(*------------------------------- byte copying ------------------------------*)

PROCEDURE Copy (src, dest: ADDRESS;  len: INTEGER) =
  BEGIN
    EVAL Cstring.memcpy (dest, src, len);
  END Copy;

PROCEDURE Zero (dest: ADDRESS;  len: INTEGER) =
  BEGIN
    EVAL Cstring.memset (dest, 0, len);
  END Zero;

(*------------------------------- rounded arithmetic ------------------------*)

PROCEDURE Align (a: ADDRESS; y: INTEGER): ADDRESS =
  BEGIN 
    RETURN LOOPHOLE (Upper (LOOPHOLE (a, INTEGER), y), ADDRESS);
  END Align;

PROCEDURE Upper (x, y: INTEGER): INTEGER =
  BEGIN
    RETURN ((x + y - 1) DIV y) * y;
  END Upper;

(*------------------------------- runtime error reporting -------------------*)

PROCEDURE FatalError (file            : TEXT;
                      line            : INTEGER;
                      msgA, msgB, msgC: TEXT      := NIL) =
  BEGIN
    StartError (msgA, msgB, msgC);
    IF (file # NIL) THEN
      RTIO.PutText ("\n***    file \"");
      RTIO.PutText (file);
      RTIO.PutText ("\", line ");
      RTIO.PutInt  (line);
    END;
    EndError (crash := TRUE);
  END FatalError;

PROCEDURE FatalErrorS (file            : ADDRESS;
                       line            : INTEGER;
                       msgA, msgB, msgC: TEXT      := NIL) =
  BEGIN
    StartError (msgA, msgB, msgC);
    IF (file # NIL) THEN
      RTIO.PutText   ("\n***    file \"");
      RTIO.PutString (file);
      RTIO.PutText   ("\", line ");
      RTIO.PutInt    (line);
    END;
    EndError (crash := TRUE);
  END FatalErrorS;

PROCEDURE FatalErrorI (msg: TEXT := NIL;  i: INTEGER) =
  BEGIN
    StartError (msg);
    RTIO.PutInt (i);
    EndError (crash := TRUE);
  END FatalErrorI;

PROCEDURE FatalErrorPC (pc: INTEGER; msgA, msgB, msgC: TEXT := NIL) =
  BEGIN
    ErrorPC (pc, msgA, msgB, msgC, crash := TRUE);
  END FatalErrorPC;

PROCEDURE ReportErrorPC (pc: INTEGER; msgA, msgB, msgC: TEXT := NIL) =
  BEGIN
    ErrorPC (pc, msgA, msgB, msgC, crash := FALSE);
  END ReportErrorPC;

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
  END StartError;

PROCEDURE EndError (crash: BOOLEAN) =
  BEGIN
    RTIO.PutText ("\n***\n\n");
    RTIO.Flush ();
    IF crash THEN RTProcess.Crash (NIL); END;
  END EndError;

BEGIN
END RTMisc.
