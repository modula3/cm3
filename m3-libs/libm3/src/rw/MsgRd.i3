(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Nov  7 13:55:50 PST 1994 by wobber     *)

(*
A "MsgRd.T" is a reader which presents the abstraction of a stream of
messages.  A message is a (possibly zero length) sequence of bytes
terminated by an end of message marker.  The reader is initially
positioned to the start of the first message.  When the end of message
marker is encountered, it is represented by "EndOfFile" on the reader.
The "nextMsg" method can be used to advance to the next message in the
stream. This method waits for the next message and returns "TRUE" when
it becomes available.  A return value of "FALSE" indicates that there
are (and will be) no further messages.  The reader's current position
is set to zero on return from "nextMsg", and the reader no longer reports
"EndOfFile" (unless of course the next message is zero length).

If "nextMsg" is invoked when the reader is not at "EndOfFile", the
remaining bytes in the current message will be skipped.

Calling "Rd.Close" on a "MsgRd.T" will release all associated resources,
and trigger checked runtime errors on further attempts to read from the
closed reader.
*)

INTERFACE MsgRd;

IMPORT Thread, Rd;

TYPE
  T = Rd.T OBJECT METHODS
    nextMsg() : BOOLEAN RAISES {Rd.Failure, Thread.Alerted};
  END;
(* The "nextMsg" method advances to the next message. A return value
   of "TRUE" indicates the presence of a new message. "FALSE" indicates
   that no next message is present, and that the end of the stream of
   messages has been reached. *)
      
(* The "nextMsg" methods affects the abstract reader state as follows:
    
\begin{tabular}{ll}
        & \\
        "len(rd)"          &   is the length of the next message \\
        "src(rd)"          &   is the contents of the next message \\
        "cur(rd)"          &   is zero \\
        "avail(rd)"        &   is unspecified \\
        "closed(rd)"       &   is unchanged \\ 
        "seekable(rd)"     &   is unchanged \\
        "intermittent(rd)" &   is unchanged \\
        & \\
\end{tabular}

   Message reader buffers must be word-aligned in memory. More
   precisely, if byte "i" in the data stream is stored in the
   buffer at memory address "j", then "i" and "j" must be
   equal modulo the machine word size.
*)

END MsgRd.

    
