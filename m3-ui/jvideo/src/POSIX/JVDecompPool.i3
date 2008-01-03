(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 27 13:01:15 PST 1995 by msm      *)
(*      modified on Sun Oct 24 17:59:00 PDT 1993 by sfreeman *)

(* this interface provides shared access to a set of JVDecomps and JVSinks.
   When a caller asks for a particular type of connection to a jv source,
   the pool will try to find an existing Sink and Decomp which match the
   parameters or create new ones.  This done, it links the Sink and Decomp
   and returns the Decomp. *)

INTERFACE JVDecompPool;

IMPORT JVBuffer, JVDecomp, JVSink, Jvs, OSError, Thread;


PROCEDURE GetDecomp (hostname: TEXT;          (* name of source host *)
                     quality : JVSink.Quality := JVSink.DefaultQuality;
                     (* transmission quality *)
                     READONLY dparams: Jvs.DcmpParams;
                     READONLY cmap   : Jvs.ColormapInfo;
                              create                      := TRUE;
                     maxSinkBuffs, maxDecompBuffs: CARDINAL := 2;
                     decompFactory: JVBuffer.Factory := NIL;
                     decompServer : Jvs.T            := NIL;
                     delay: CARDINAL := 0;
		     subtype: CARDINAL := 0): JVDecomp.T
  RAISES {OSError.E, Thread.Alerted};
(* try to find an exisiting Sink/Decomp pair which match the given
   parameters.  If either is not found then, create some if "create" is
   TRUE, otherwise return NIL.

   "maxSinkBuffs" and "maxDecompBuffs" set the maximum sizes for the buffer
   pools if they are created.

   "decompFactory" is used for creating new decompression.  buffers.  If it
   is NIL, then a new one will be created; if "decompServer" is non-NIL,
   then that will be used, otherwise a new one will be created.  If
   "decompFactory" is non-NIL, then "decompServer" must be the same one
   used in "decompFactory"

   the caller should call decomp.join() and pool.join() to register an
   interest with the returned decomp and pool *)

END JVDecompPool.
