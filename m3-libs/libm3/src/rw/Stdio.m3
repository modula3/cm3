(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 19 08:30:00 PST 1995 by kalsow         *)
(*      modified on Mon Oct 11 11:04:30 PDT 1993 by wobber         *)
(*      modified on Wed Apr 21 17:30:38 PDT 1993 by mcjones        *)
(*      modified on Mon Feb 22 10:06:11 PST 1993 by jdd            *)
(*      modified on Fri Feb 19 11:29:24 PST 1993 by mjordan        *)
(*      modified on Fri Feb  7 00:04:41 PST 1992 by muller         *)


MODULE Stdio;

IMPORT File, FileRd, FileWr, OSError, Process, Thread, Wr, WrClass;

(*****************************************************************
  The shutdown routine used to call Unsafe{Rd,Wr}.FastClose to
  flush and close the streams.  But, it's an error to close the
  file descriptors that we did't open.  So, we'll just flush.
PROCEDURE ShutDown () =
  <* FATAL Thread.Alerted *> 
  BEGIN
    IF stdin # NIL THEN
      TRY UnsafeRd.FastClose (stdin); EXCEPT Rd.Failure => END;
    END;
    IF stdout # NIL THEN
      TRY UnsafeWr.FastClose (stdout); EXCEPT Wr.Failure => END;
    END;
    IF stderr # NIL THEN
      TRY UnsafeWr.FastClose (stderr); EXCEPT Wr.Failure => END;
    END;
  END ShutDown;
**********************************************************************)

PROCEDURE ShutDown () =
  (* Note that this routine is unsafe.  It calls flush methods
     without acquiring the locks.  We don't acquire the locks
     because it can cause a deadlock when we're trying to
     crash the program from an arbitrary state. *)
  BEGIN
    TRY
      IF (stdout # NIL) AND (NOT stdout.closed) THEN stdout.flush (); END;
      IF (stderr # NIL) AND (NOT stderr.closed) THEN stderr.flush (); END;
    EXCEPT Thread.Alerted, Wr.Failure =>
      (* oh well, we tried. *)
    END;
  END ShutDown;

BEGIN
  stdin  := NIL;
  stdout := NIL;
  stderr := NIL;
  bufferedStderr := NIL;
  
  <*FATAL OSError.E*>
  VAR hIn, hOut, hErr: File.T;
  BEGIN
    Process.GetStandardFileHandles(stdin:=hIn, stdout:=hOut, stderr:=hErr);
    IF hIn # NIL THEN
      stdin := NEW(FileRd.T).init(hIn);
    END;
    IF hOut # NIL THEN
      stdout := NEW(FileWr.T).init(hOut, buffered := TRUE);
    END;
    IF hErr # NIL THEN
      stderr := NEW(FileWr.T).init(hErr, buffered := FALSE);
      bufferedStderr := NEW(FileWr.T).init(hErr, buffered := TRUE);
    END;
  END;
  
  Process.RegisterExitor (ShutDown);
END Stdio.
