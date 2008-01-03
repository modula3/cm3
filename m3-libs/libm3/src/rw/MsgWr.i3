(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Nov  7 14:01:34 PST 1994 by wobber     *)

(*
A "MsgWr.T" is a writer which presents the abstraction of a stream of
messages.  A message is a (possibly zero length) sequence of bytes
terminated by an end of message marker.  The writer is initially
positioned to the start of the first message.  The "nextMsg" method
can be used to end the current message, and position the writer at
the start of the next message.  The writer's current position is
reset to zero on return from "nextMsg". 

Invoking "Wr.Flush" on a "MsgWr.T" will flush the current buffer to the
abstract writer target, but will not end the current message.

Calling "Wr.Close" on a "MsgWr.T" will release all associated resources
and trigger checked runtime errors on further attempts to write to the
closed writer. It also flushes and terminates the current message.  This
means that a zero-length message will be sent at close time if no data 
has been written into the current message (e.g. directly after "nextMsg"
or object initialization).
*)

INTERFACE MsgWr;

IMPORT Thread, Wr;

TYPE
  T = Wr.T OBJECT METHODS
    nextMsg() RAISES {Wr.Failure, Thread.Alerted};
  END;
(* "nextMsg" ends the current message, and starts the next. *)

(* The "nextMsg" method affects the abstract writer state as follows:
      
\begin{tabular}{ll}
        & \\
          "len(wr)"      &      is zero \\
          "c(wr)"        &      is empty \\
          "cur(wr)"      &      is zero \\
          "target(wr)"   &      is empty \\
          "closed(wr)"   &      is unchanged \\
          "seekable(wr)" &      is unchanged \\
          "buffered(wr)" &      is unchanged \\
        & \\
\end{tabular}

   Message writer buffers must be word-aligned in memory. More
   precisely, if byte "i" in the data stream is stored in the
   buffer at memory address "j", then "i" and "j" must be
   equal modulo the machine word size.
*)

END MsgWr.

    

    
