INTERFACE ProcessFrequencyModulation;
(* author: thielema *)

IMPORT Signal, SignalControl, Interpolation;
IMPORT Thread;

TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x            : Signal.T;
                   mod          : SignalControl.T;
                   interpolation: Interpolation.T; ): T
                   RAISES {Signal.End, Signal.Error, Thread.Alerted};
           END;

END ProcessFrequencyModulation.
