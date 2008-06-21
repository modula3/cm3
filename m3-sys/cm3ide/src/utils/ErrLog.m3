(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb  8 13:39:27 PST 1995 by kalsow                   *)

MODULE ErrLog;

IMPORT Wr, Thread, IO; 

VAR
  consoleWr: Wr.T     := NIL;

PROCEDURE Redirect (wr: Wr.T) = 
  BEGIN
    consoleWr := wr;
  END Redirect;

PROCEDURE CancelRedirect () =
  BEGIN
    consoleWr := NIL;
  END CancelRedirect;

PROCEDURE Note (msg: TEXT) =
  BEGIN
    Msg (msg);
  END Note;

PROCEDURE Msg (a, b, c, d: TEXT := NIL) =
  VAR result: TEXT := "";
  BEGIN
    IF (a # NIL) THEN result := result & a; END;
    IF (b # NIL) THEN result := result & b; END;
    IF (c # NIL) THEN result := result & c; END;
    IF (d # NIL) THEN result := result & d; END;

    LOCK log_mu DO
      IF consoleWr # NIL THEN
        TRY
          Wr.PutText (consoleWr, result);
          Wr.PutText (consoleWr, Wr.EOL);
          Wr.Flush (consoleWr);
        EXCEPT
        | Thread.Alerted, Wr.Failure => (* ignore *)
        END
      ELSE
        IO.Put (result);
        IO.Put (Wr.EOL);
      END;

      (* remember the message in the exported log *)
      log [log_head] := result;
      INC (log_head);  IF (log_head > LAST (log)) THEN log_head := 0; END;
      IF (log_len < NUMBER (log)) THEN INC (log_len); END;
    END;
  END Msg;

BEGIN
  log_mu := NEW (MUTEX);
END ErrLog.
