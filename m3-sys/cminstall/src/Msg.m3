(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE Msg;

IMPORT AtomList, FileWr, OS, OSError, Process, Rd, Stdio;
IMPORT Text, Text2, TextWr, Thread, Wr;

CONST
  SupportMsg =
    "Please feel free to contact support@cmass.com to troubleshoot this problem.";

VAR
  log_wr: Wr.T := TextWr.New ();
  got_drain: BOOLEAN := FALSE;

PROCEDURE AskBool (question, default: TEXT): BOOLEAN =
  VAR txt := Ask (question, default);
  BEGIN
    LOOP
      IF txt = NIL THEN
        (* huh? *)
      ELSIF Text.Equal (txt, "y") OR Text.Equal (txt, "Y")
        OR Text.Equal (txt, "yes") OR Text.Equal (txt, "YES") THEN
        RETURN TRUE;
      ELSIF Text.Equal (txt, "n") OR Text.Equal (txt, "N")
        OR Text.Equal (txt, "no") OR Text.Equal (txt, "NO") THEN
        RETURN FALSE;
      END;
      Out ("");
      Out ("Please enter Y (yes) or N (no).");
      txt := Ask (question, default);
    END;
  END AskBool;

PROCEDURE Ask (question, default: TEXT): TEXT =
  VAR txt: TEXT;  q_len := Text.Length (question);
  BEGIN
    OutX (question);
    IF Text.GetChar (question, q_len - 1) # ' ' THEN  OutX (" ");  END;
    IF default # NIL THEN
      IF q_len + Text.Length (default) >= 70 THEN OutX (Wr.EOL); OutX ("   "); END;
      OutX ("[");  OutX (default);  OutX ("] ");
    END;
    FlushX ();
    TRY
      txt := Rd.GetLine (Stdio.stdin);
    EXCEPT Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
      (* ignore *)
    END;
    IF (txt # NIL) THEN txt := Text2.Trim (txt); END;
    IF (txt = NIL) OR Text.Length (txt) <= 0 THEN
      OutLog ("");
      txt := default;
    ELSE
      OutLog (txt);
    END;
    RETURN txt;
  END Ask;

PROCEDURE Debug (a, b, c, d: TEXT := NIL) =
  BEGIN
    IF Debugging THEN Out (a, b, c, d); END;
  END Debug;

PROCEDURE Warn (a, b, c, d: TEXT := NIL) =
  BEGIN
    Out (Wr.EOL, "Warning: ", a, b, c, d);
    Out (Wr.EOL, SupportMsg, Wr.EOL);
  END Warn;

PROCEDURE Error (ec: AtomList.T;  a, b, c, d: TEXT := NIL) =
  VAR e: TEXT := NIL;
  BEGIN
    IF (ec # NIL) THEN e := OS.Err (ec); END;
    Out (Wr.EOL & "Unexpected problem: ", a, b, c, d, e);
    Out (Wr.EOL, SupportMsg, Wr.EOL);
    Process.Exit (1);
  END Error;

PROCEDURE Out (a, b, c, d, e, f: TEXT := NIL) =
  BEGIN
    OutX (a);  OutX (b);
    OutX (c);  OutX (d);
    OutX (e);  OutX (f);
    OutX (Wr.EOL);
    FlushX ();
  END Out;

PROCEDURE OutX (a: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    IF (a # NIL) THEN
      Wr.PutText (log_wr, a);
      Wr.PutText (Stdio.stdout, a);
    END;
  END OutX;

PROCEDURE OutLog (a: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText (log_wr, a);
    Wr.PutText (log_wr, Wr.EOL);
  END OutLog;

PROCEDURE FlushX () =
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.Flush (log_wr);
    Wr.Flush (Stdio.stdout);
  END FlushX;

PROCEDURE AttachDrain (filename: TEXT) =
  VAR txt: TEXT;  drain: Wr.T;
  BEGIN
    (* open the log file *)
    TRY
      drain := FileWr.Open (filename);
    EXCEPT
    | OSError.E (ec) =>
      Error (ec, "Unable to open the log file: ", filename);
      RETURN;
    END;

    (* dump everything we've got so far *)
    txt := TextWr.ToText (log_wr);
    TRY
      Wr.PutText (drain, txt);
      Wr.Flush (drain);
    EXCEPT
    | Wr.Failure (ec) =>
        OutLog (txt);  (* restore the in-memory log *)
        Error (ec, "Unable to write the log file: ", filename);
        drain := NIL;
        RETURN;
    | Thread.Alerted =>
        OutLog (txt);  (* restore the in-memory log *)
        Error (NIL, "Interrupted while writing the log file: ", filename);
        drain := NIL;
        RETURN;
    END;

    (* ok, we successfully converted *)
    log_wr := drain;
    got_drain := TRUE;
  END AttachDrain;

PROCEDURE FinishLog (filename: TEXT) =
  BEGIN
    IF NOT got_drain THEN
      OS.WriteFile (filename, TextWr.ToText (log_wr));
    END;
  END FinishLog;

BEGIN
END Msg.
