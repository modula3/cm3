(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Oct 25 12:31:49 PDT 1993 by sfreeman *)

(* accepts input from a JVConverter and decompresses it into its own buffer
   pool.  These are immutable.  If you want different parameters, close
   this one and open a new one *)

INTERFACE JVDecomp;

IMPORT Atom, JVBuffer, JVConverter, Jvs, OSError, Point, Thread, Tick;

VAR decompError: Atom.T;         (* error has come from this module *)

TYPE
  T <: Public;
  Public =
    JVConverter.T OBJECT
    METHODS
      init (         in        : JVConverter.T;
            READONLY dparams   : Jvs.DcmpParams;
            READONLY cmap      : Jvs.ColormapInfo;
                     maxBuffers: CARDINAL           := 2;
                     factory   : JVBuffer.Factory   := NIL;
                     server    : Jvs.T              := NIL  ): T
            RAISES {OSError.E, Thread.Alerted};
      (* initial setup of T.  "in" provides the frames to decompress.  The
         parameters specify the type of frames to be returned.
         "maxBuffers" specifies the maximum number of buffers allowed in
         the output buffer pool.  "factory" is used for creating new
         buffers.  If it is NIL, then a new one will be created; if
         "server" is non-NIL, then that will be used, otherwise a new one
         will be created.  If "factory" is non-NIL, then "server" must be
         the same one used in "factory" *)

      outSize (): Point.T;
      (* A Jvs.T can only decompress to certain sizes, so call outSize to
         find out what size the buffers really are.  If the converter is
         dead, or not yet ready, returns Point.Origin *)

      getInput (): JVConverter.T;
      (* returns input converter associated with the T *)
    END;

(* -- statistics -- *)

TYPE
  Statistics =
    JVConverter.Statistics OBJECT
      cumLatency: Tick.T;
      (* accumulation of difference between when the last pipeline stage
         finished with a buffer and this stage received it *)
    END;

END JVDecomp.
