(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb  8 13:39:27 PST 1995 by kalsow                   *)

MODULE ErrLog;

IMPORT Stdio, Wr, Thread;

PROCEDURE Note (msg: TEXT) =
  BEGIN
    Msg (msg);
  END Note;

PROCEDURE Msg (a, b, c, d: TEXT := NIL) =
  VAR wr := Stdio.stderr;
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      IF (a # NIL) THEN Wr.PutText (wr, a); END;
      IF (b # NIL) THEN Wr.PutText (wr, b); END;
      IF (c # NIL) THEN Wr.PutText (wr, c); END;
      IF (d # NIL) THEN Wr.PutText (wr, d); END;
      Wr.PutChar (wr, '\n');
      Wr.Flush (wr);
    EXCEPT Wr.Failure, Thread.Alerted =>
      (* skip *)
    END;
  END Msg;

BEGIN
END ErrLog.
