(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:12:28 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

(* This interface contains procedures for reading and writing logs
    for stable objects. Logs are written on "Wr.T"'s and read from "Rd.T"'s.

    To log procedures (methods usually) first "OutCall()" has to
    be called to log that the method identified by a "CARDINAL" code
    was called in the program. Afterwards the parameters of the
    method are logged by calling the corresponding "Out[Type]()"
    procedures of this interface. Now the method itself has to be
    called. If it returned (with or without exceptions),
    "OutCallEndMark()" has to be called to log the termination. 

    To recover, the analogous log reading procedures of this interface
    are called: First "InCall()" gets the code of the logged
    method followed by (possibly more than one) "In[Type]()" 
    to get the parameter values
    with which the method was originally called. Afterwards a call to
    "CheckCallEndMark()" checks wether the logged method
    terminated normally. If so, the recovery-procedure calls
    the logged method with the parameters just read. If not, the
    recovery procedure terminates (since this must be the
    end of the log).

   \paragraph{Exceptions}
   Reading something that does not correspond to the requested
   type or reading beyond the end of file is reported as exception
   "Error". This might indicate a protocol error or just that the
   logged program was interrupted during writing the log.
   Any other reading exceptions cause a "StableError.Halt()".

   There are no writing procedures exceptions
   since there can be no code to handle them. These procedures
   are called inside generated procedures which can not pass them
   nor handle them in a meaningful way. Thus all exceptions lead
   to a "StableError.Halt".
*)

INTERFACE StableLog;

IMPORT Rd, Wr;

EXCEPTION Error;

PROCEDURE OutCall(log: Wr.T; procId: CARDINAL);
(* Mark the beginning of a logged procedure-call. Procedures are
     identified by positive numbers (probably enumeration codes).

    "OutCall()" has to be called {\em before} logging the procedure 
    parameters. *)

PROCEDURE OutCallEndMark(log: Wr.T);
(* Marks the successful completion of a logged procedure. Without
    this mark a call to a procedure is seen as unsuccessful. Such
    calls mark a possible end of a log, they will not be repeated on
    recoveries. *)

PROCEDURE InCall(log: Rd.T; max: CARDINAL): CARDINAL
    RAISES {Error};
(* Read a procedure code as written with "OutCall()" from the
   log. Exception "Error" is raised if the number is greater than
   "max". *)

PROCEDURE CheckCallEndMark(log: Rd.T): BOOLEAN;
(* Check if the successful end of the call to a procedures was logged.
    Return "TRUE", if so. Return of "FALSE" marks the end of a
    logfile (the call to the procedure which parameters were just read
    was not finished) *)

PROCEDURE OutRef(log: Wr.T; r: REFANY);
(* Marshal the data structure reachable from "r" and write it to the log. *)

PROCEDURE InRef(log: Rd.T): REFANY RAISES {Error};
(* Unmarshal a marshaled subtype of "REFANY" as pickled by "OutRef". *)


(* \subsubsection*{Procedures for generic parameters logging} *)

PROCEDURE OutChar(log: Wr.T; c: CHAR);
(* Marshal a char *)

PROCEDURE OutChars(
    log: Wr.T; READONLY chars: ARRAY OF CHAR);
(* Marshal a char array in native format. *)

PROCEDURE OutInteger(log: Wr.T; i: INTEGER);
(* Marshal an integer. *)

PROCEDURE OutCardinal(log: Wr.T; card: CARDINAL);
(* Marshal a cardinal. *)

PROCEDURE OutBoolean(log: Wr.T; bool: BOOLEAN);
(* Marshal a boolean value. *)

PROCEDURE OutReal(log: Wr.T; r: REAL);
(* Marshal a real in native format. *)

PROCEDURE OutLongreal(log: Wr.T; d: LONGREAL);
(* Marshal a longreal in native format. *)

PROCEDURE OutExtended(log: Wr.T; x: EXTENDED);
(* Marshal an extended in native format. *)


(* \paragraph{Logreading procedures} \  *)

PROCEDURE InChar(log: Rd.T): CHAR
    RAISES {Error};
(* Unmarshal a char. *)

PROCEDURE InCharsLen(log: Rd.T): CARDINAL
    RAISES {Error};
PROCEDURE InChars(
    log: Rd.T; VAR chars: ARRAY OF CHAR)
    RAISES {Error};
(* Unmarshal a char array. *)

PROCEDURE InInteger(
    log: Rd.T; 
    min := FIRST(INTEGER);
    max := LAST(INTEGER)): INTEGER
    RAISES {Error};
(* Unmarshal an integer, checking that its value is in  "[min..max]". *)

PROCEDURE InCardinal(
    log: Rd.T; lim: CARDINAL := LAST(CARDINAL)): CARDINAL
    RAISES {Error};
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)

PROCEDURE InBoolean(log: Rd.T): BOOLEAN
    RAISES {Error};
(* Unmarshal a boolean value. *)

PROCEDURE InReal(log: Rd.T): REAL
    RAISES {Error};
(* Unmarshal a real value. *)

PROCEDURE InLongreal(log: Rd.T): LONGREAL
    RAISES {Error};
(* Unmarshal a longreal value. *)

PROCEDURE InExtended(log: Rd.T): EXTENDED
    RAISES {Error};
(* Unmarshal an extended value. *)

END StableLog.
