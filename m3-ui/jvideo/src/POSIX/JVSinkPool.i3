(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 27 15:07:28 PST 1995 by msm      *)
(*      modified on Sun Oct 24 17:57:58 PDT 1993 by sfreeman *)

(* this interface provides shared access to a set of JVSinks.  When a
   caller asks for a particular type of Sink, the pool will try to find an
   existing one or create a new one if none exists. *)

INTERFACE JVSinkPool;

IMPORT JVSink, OSError, Thread;

PROCEDURE GetSink (hostname: TEXT;          (* name of source host *)
                   quality : JVSink.Quality := JVSink.DefaultQuality;
                   (* transmission quality *)
                   create               := TRUE;
                   maxBuffers: CARDINAL := 2;
                   delay: CARDINAL := 0): JVSink.T
  RAISES {OSError.E, Thread.Alerted};
(* try to find an existing sink which matches the given parameters If one
   exists, then return it.  If none exists, then if "create" then make a
   new one otherwise return NIL.

   the "maxBuffers" field is used during the creation of a buffer pool for
   a new sink and specifies the maximum size of the pool.

   the caller should call sink.join() and pool.join() to register an
   interest with the returned sink and pool *)

END JVSinkPool.
