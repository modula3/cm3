(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 18 11:53:02 PDT 1993 by wobber                   *)
(*      created  on ???????????????????????????? by unknown                  *)

INTERFACE RdCopy;

IMPORT Rd, Wr, Thread;

PROCEDURE ToWriter (rd: Rd.T; wr: Wr.T; length: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {Rd.Failure, Wr.Failure, Thread.Alerted};
  (* Copies MIN(length, len(rd) - cur(rd)) bytes from rd to wr.  Returns
     the number of bytes copied.  Equivalent to:

     FOR i := 0 TO length - 1 DO
       IF Rd.EOF(rd) THEN RETURN i; END;
       Wr.PutChar(Rd.GetChar(rd));
     END;
     RETURN length;

     But faster.  This uses ToProc below to copy from rd's buffer
     into wr's putString method.
   *)

PROCEDURE ToProc (rd  : Rd.T;
                  proc: PROCEDURE (READONLY a: ARRAY OF CHAR) RAISES ANY;
                  length: CARDINAL := LAST(CARDINAL)): CARDINAL
  RAISES ANY (* RAISES(proc) + {Rd.Failure, Thread.Alerted *);
  (* Passes MIN(length, len(rd) - cur(rd)) bytes from rd to proc.  Returns
     the number of bytes copied.  The length of the arrays passed to
     proc is not defined. *)

PROCEDURE FromProc (wr: Wr.T;
                    proc: PROCEDURE (VAR a: ARRAY OF CHAR): CARDINAL
                            RAISES ANY;
                    length: CARDINAL := LAST(CARDINAL)): CARDINAL
  RAISES ANY (* RAISES(proc) + {Rd.Failure, Thread.Alerted *);
  (* Calls proc repeatedly to fill in up to length bytes in a buffer which are
     written to the writer.  If proc returns a value < NUMBER(a) then
     the procedure is assumed to be finished producing characters.
     Returns the total number of characters gotten from proc. *)

  

END RdCopy.
