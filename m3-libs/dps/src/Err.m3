(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 14 12:42:21 PST 1994 by kalsow                   *)

MODULE Err;

IMPORT Stdio, Wr, Thread;

PROCEDURE Msg (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stderr, a); END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stderr, b); END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stderr, c); END;
    IF (d # NIL) THEN Wr.PutText (Stdio.stderr, d); END;
    Wr.PutChar (Stdio.stderr, '\n');
    Wr.Flush (Stdio.stderr);
  END Msg;

BEGIN
END Err.
