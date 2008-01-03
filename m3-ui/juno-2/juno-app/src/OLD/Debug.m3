(* Copyright (C) 1992, Digital Equipment Corporation *)
(* Last modified on Fri Aug 7 21:51:59 PDT 1992 by myers *)

MODULE Debug;

IMPORT Wr, Stdio, Thread;

PROCEDURE Print (t: TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText(Stdio.stderr, t);
    Wr.PutChar(Stdio.stderr, '\n');
    Wr.Flush(Stdio.stderr)
  END Print;

BEGIN
END Debug.
