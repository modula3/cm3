(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Error.m3                                              *)
(* Last modified on Tue Jan 31 08:07:00 PST 1995 by kalsow     *)
(*      modified on Fri Mar 22 08:29:36 1991 by muller         *)

MODULE Error;

IMPORT Fmt, M3ID, M3Buf, Host, Scanner, M3CG;

TYPE
  Level = [0..3];

TYPE
  IgnoreCell = REF RECORD offs: INTEGER; next: IgnoreCell END;

CONST
  Labels = ARRAY Level OF TEXT {
     "info: ",     (* informational messages *)
     "warning: ",  (* "fussy" warnings *)
     "warning: ",  (* warnings *)
     ""            (* errors *)
  };

VAR
  count   := ARRAY Level OF INTEGER {0, ..};
  spare   : M3Buf.T    := NIL;
  ignores : IgnoreCell := NIL;

PROCEDURE Msg (msg: TEXT) =
  VAR wr := Header ();
  BEGIN
    Out (wr, msg);
    Trailer (wr);
  END Msg;

PROCEDURE Int (n: INTEGER;  msg: TEXT) =
  VAR wr := Header ();
  BEGIN
    Out (wr, msg);
    Out (wr, " (");
    Out (wr, Fmt.Int (n));
    Out (wr, ")");
    Trailer (wr);
  END Int;

PROCEDURE ID (id: M3ID.T;  msg: TEXT) =
  VAR wr := Header ();
  BEGIN
    Out  (wr, msg);
    Out  (wr, " (");
    OutS (wr, id);
    Out  (wr, ")");
    Trailer (wr);
  END ID;

PROCEDURE Txt (id, msg: TEXT) =
  VAR wr := Header ();
  BEGIN
    Out (wr, msg);
    Out (wr, ": ");
    Out (wr, id);
    Trailer (wr);
  END Txt;

PROCEDURE QID (READONLY q: M3CG.QID;  msg: TEXT) =
  VAR wr := Header ();
  BEGIN
    Out (wr, msg);
    Out (wr, " (");
    IF (q.module # M3ID.NoID) THEN
      OutS (wr, q.module);
      Out  (wr, ".");
    END;
    OutS (wr, q.item);
    Out (wr, ")");
    Trailer (wr);
  END QID;

PROCEDURE Info (msg: TEXT) =
  BEGIN
    IF Toss (FIRST (Level)) THEN RETURN END;
    VAR wr := Header (FIRST (Level)); BEGIN
      Out (wr, msg);
      Trailer (wr);
    END;
  END Info;

PROCEDURE InfoInt (n: INTEGER;  msg: TEXT) =
  BEGIN
    IF Toss (FIRST (Level)) THEN RETURN END;
    VAR wr := Header (FIRST (Level)); BEGIN
      Out (wr, msg);
      Out (wr, " (");
      Out (wr, Fmt.Int (n));
      Out (wr, ")");
      Trailer (wr);
    END;
  END InfoInt;

PROCEDURE Warn (level: INTEGER;  msg: TEXT) =
  BEGIN
    IF Toss (level) THEN RETURN END;
    VAR wr := Header (level); BEGIN
      Out (wr, msg);
      Trailer (wr);
    END;
  END Warn;

PROCEDURE WarnID (level: INTEGER;  id: M3ID.T;  msg: TEXT) =
  BEGIN
    IF Toss (level) THEN RETURN END;
    VAR wr := Header (level); BEGIN
      Out  (wr, msg);
      Out  (wr, " (");
      OutS (wr, id);
      Out  (wr, ")");
      Trailer (wr);
    END;
  END WarnID;

PROCEDURE Header (level: INTEGER := LAST (INTEGER)): M3Buf.T =
  VAR wr: M3Buf.T;
  BEGIN
    IF (spare # NIL)
      THEN wr := spare;  spare := NIL;
      ELSE wr := M3Buf.New ();
    END;
    level := MAX (FIRST (Level), MIN (level, LAST (Level)));
    INC (count[level]);
    Out  (wr, Labels [level]);
    RETURN wr;
  END Header;

PROCEDURE Trailer (wr: M3Buf.T) =
  VAR n: INTEGER := 0;  file: TEXT;  line: INTEGER;
  BEGIN
    Scanner.Here (file, line);
    Host.env.report_error (file, line, M3Buf.ToText (wr));
    spare := wr;
    IF (Host.errorDie >= 0) THEN
      FOR i := FIRST (count) TO LAST (count) DO INC (n, count[i]) END;
      IF (n >= Host.errorDie) THEN <* ASSERT FALSE *> END;
    END;
  END Trailer;

PROCEDURE Out (wr: M3Buf.T;  t: TEXT) =
  BEGIN
    IF (t # NIL) THEN M3Buf.PutText (wr, t); END;
  END Out;

PROCEDURE OutS (wr: M3Buf.T;  id: M3ID.T) =
  BEGIN
    M3ID.Put (wr, id);
  END OutS;

PROCEDURE Count (VAR nErrors, nWarnings: INTEGER) =
  BEGIN
    nErrors := count [LAST (count)];
    nWarnings := 0;
    FOR i := FIRST (count) + 1 TO LAST (count) - 1 DO
      INC (nWarnings, count[i]);
    END;
  END Count;

PROCEDURE IgnoreWarning (offset: INTEGER) =
  BEGIN
    WITH i = NEW (IgnoreCell) DO
      i.offs := offset;
      i.next := ignores;
      ignores := i;
    END;
  END IgnoreWarning;

PROCEDURE Toss (level: INTEGER): BOOLEAN =
  VAR i: IgnoreCell;  here: INTEGER;
  BEGIN
    IF (level < Host.warnings) THEN RETURN TRUE END;
    here := Scanner.offset;
    i := ignores;
    WHILE (i # NIL) DO
      IF (i.offs = here) THEN RETURN TRUE END;
      i := i.next;
    END;
    RETURN FALSE;
  END Toss;

PROCEDURE Reset () =
  BEGIN
    ignores := NIL;
    FOR i := FIRST (count) TO LAST (count) DO count[i] := 0 END;
  END Reset;

BEGIN
END Error.
