(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep  2 15:41:31 PDT 1993 by sfreeman *)

INTERFACE JVConverterF;

IMPORT JVBuffer, JVConverter, Thread;

REVEAL
  JVConverter.Private =
    MUTEX BRANDED OBJECT
      clients   : CARDINAL           := 0;
      paused    : CARDINAL           := 0;
      pauseEvent: Thread.Condition;

      output: JVBuffer.Pool := NIL; (* used to pass buffers to clients *)
      statistics: JVConverter.Statistics := NIL;
      (* collect statistics when "statistics" # NIL *)
    METHODS
      (* all methods LL < self *)
      init (): JVConverter.T;    (* initialise base parts of
                                    JVConverter.T *)

      start () RAISES {JVConverter.Error, Thread.Alerted};
      stop  () RAISES {JVConverter.Error, Thread.Alerted};
      (* start and stop the processing thread.  These must be overriden by
         a subtype *)

      startStats ();             (* initialise and start collecting
                                    statistics *)
      stopStats ();              (* stop collecting statistics *)
      getStats  (): JVConverter.Statistics;
      (* get the current values of the statistics. may return NIL *)
    END;

END JVConverterF.
