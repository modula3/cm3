(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Mar 16 12:22:38 PST 1994 by wobber *)

INTERFACE TCPSpecial;

IMPORT TCP, IP, Thread;

(* procedures *)

PROCEDURE EOF(t: TCP.T) : BOOLEAN;

(* Returns "TRUE" if and only if there are no more bytes to be read from
   this connection, and the connection indicates end-of-file (e.g.
   the other side closed it. *)

PROCEDURE StartConnect(ep: IP.Endpoint) : TCP.T
    RAISES {IP.Error};

(* "StartConnect" initiates a request to connect to the destination
   specified by "ep".  If "msgT" is "TRUE" then the results reader
   and writer fields are of type "MsgRd.T" and "MsgWr.T".  Otherwise,
   they are a normal reader and writer.  The resulting "T" can be used
   to make read and write calls, but these may result in errors if
   in fact the connection attempt is not successful.  *)

PROCEDURE FinishConnect(
    t: TCP.T; waitFor: LONGREAL := -1.0D0) : BOOLEAN
    RAISES {IP.Error, Thread.Alerted};

(* "FinishConnect" returns a "BOOLEAN" to indicate if a connection
   request initiated via "StartConnect" has successfully completed.
   A result of "TRUE" indicates that it has.  "FALSE" means that the
   connection request is still outstanding.  If "waitFor" is negative,
   then "FinishConnect" waits indefinitely until the operation completes,
   otherwise it waits for a maximum of "waitFor" seconds.  The caller
   should continue to call this procedure until it either returns "TRUE"
   or raises an error. *)

END TCPSpecial.
