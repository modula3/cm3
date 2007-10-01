INTERFACE ProcessDelay;
(* author: thielema *)

IMPORT Signal;
IMPORT Thread;

TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x: Signal.T; delay: INTEGER; ): T
                   RAISES {Signal.End, Signal.Error, Thread.Alerted};
           END;

END ProcessDelay.
