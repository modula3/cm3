(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Wed Mar 16 12:31:58 PST 1994 by wobber *)

INTERFACE ConnFD;

IMPORT Rd, Wr, Thread;

TYPE T = TRep;

(* A "T" is a bi-directional communications channel (for example
   a TCP channel).  Given an "T", a client can initialize a
   paired reader and writer whose source and target are the channel. *)
       
(* ultimately, the type above will be a subtype of "File.T" *)

EXCEPTION TimedOut;

TYPE
  TRep = MUTEX OBJECT METHODS
    get(VAR arr: ARRAY OF CHAR; waitFor: LONGREAL := -1.0D0) : CARDINAL
        RAISES {Rd.Failure, Thread.Alerted, TimedOut};
    put(VAR arr: ARRAY OF CHAR)
        RAISES {Wr.Failure, Thread.Alerted};
    shutdownIn() RAISES {Rd.Failure};
    shutdownOut() RAISES {Wr.Failure};
    close();
  END;

  (* Get method:
        If "timeout" is negative, "get" will block forever.
        Otherwise, it will block for a maximum of of "waitFor"
        seconds. *)

END ConnFD.

















