(* Copyright 1996, Critical Mass, Inc.   All rights reserved. *)

(* A "LineWr.T", or line writer, is a writer that passes its
   buffer, a line at a time, to a user supplied procedure.
   Line writers are unbuffered, unseekable, and never raise
   "Failure" or "Alerted". *)

INTERFACE LineWr;

IMPORT Wr, Thread;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS
    init (proc: CallBack;  ref: REFANY): T;
  END;

  CallBack = PROCEDURE (ref: REFANY;  line: TEXT)
               RAISES {Wr.Failure, Thread.Alerted};

(* The call "wr.init(p,r)" initializes "wr" to be a line writer with
   an empty buffer that will call "p(r,buf)" for each line of data
   put into the buffer.  Carriage return and linefeed characters
   will be stripped from the data passed to "p". *)

PROCEDURE New (proc: CallBack;  ref: REFANY): T;
(* Equivalent to "NEW(T).init(proc,ref)". *)

PROCEDURE Clear (t: T);
(* == Wr.Flush (t) without raising exceptions *)

END LineWr.

