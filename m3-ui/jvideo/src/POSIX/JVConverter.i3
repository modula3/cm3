(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep  2 15:47:42 PDT 1993 by sfreeman *)

(* this is the superclass for objects which provide a stage in the
   conversion or displaying of video or audio information *)

INTERFACE JVConverter;

IMPORT AtomList, JVBuffer, Thread;

EXCEPTION Error(AtomList.T);

TYPE
  T <: Public;
  Private <: ROOT;
  Public =
    Private OBJECT
    METHODS
      join  () RAISES {Error, Thread.Alerted};
      leave () RAISES {Error, Thread.Alerted};
      (* clients of the object should call join() to register their
         interest.  When a client joins after there have been none, a
         thread is started to process input.  When the last client leaves,
         the processing thread will be stopped and things cleaned up.

         It is a checked runtime error to call leave more often than
         join. *)

      setPaused (paused := FALSE);
      (* tell the T that one of the clients is not interested in input for
         the moment, but doesn't want to disconnect.  When all the clients
         are paused, the T will stop accepting input.  This is implemented
         as a simple counter, so each client must keep track of its own
         paused state and not set or unset it multiple times in either
         direction.

         It is a checked runtime error if there are more calls to set
         paused than clients, or more calls to unset than calls to set *)

      getOutput (): JVBuffer.Pool;
      (* return the output buffer pool for the T, may return NIL *)

      close ();
      (* close everything down and clean up.  must be overriden by
         subclass *)
    END;

(* callers can describe how unhandled, but non-fatal, errors are
   reported--rather than crashing *)
TYPE ErrorReporter = OBJECT METHODS report (msg: TEXT); END;

VAR
  toStderr: ErrorReporter;       (* default error reporter, prints text to
                                    stderr *)


PROCEDURE RegisterErrorReporter (er: ErrorReporter): ErrorReporter;
(* register an error reporter and the return the previous one.  Either may
   be NIL.  The default (NIL) is to crash *)

PROCEDURE ReportError (msg: TEXT);
(* report the given message, or crash if no handler *)

(* -- statistics. these can be set and retreived from JVConverterF.i3 -- *)

TYPE
  Statistics = OBJECT
                 framesStarted, framesProcessed: CARDINAL;
                 (* number of frames attempted, and succesfully processed
                    since record last cleared *)
                 timesBlocked: CARDINAL;
                 (* number of times we had to block before being able to
                    send output *)
               END;


END JVConverter.
