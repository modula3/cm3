(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Mar 16 12:25:22 PST 1994 by wobber *)

INTERFACE HeaderOps;

IMPORT TCP, ConnFD, Rd, Wr, Thread;

TYPE Op = {Connect, Ping, PingAck, PingError};

PROCEDURE Send(t: TCP.T; op: Op; hisEP, myEP: TEXT := NIL)
    RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Receive(
    t: TCP.T;
    timeout: LONGREAL;
    VAR myEP: TEXT;
    VAR hisEP: TEXT) : Op
    RAISES {Rd.Failure, ConnFD.TimedOut, Thread.Alerted};

  (* If "timeout" is negative, "Receive" will block forever.
     Otherwise, it will block for a maximum of of "timeout"
     seconds. *)

END HeaderOps.

