INTERFACE ProcessPhaseModulation;
(* author: thielema *)

IMPORT Signal, SignalControl, Interpolation;
IMPORT Thread;

TYPE
  T <: Public;
  Public =
    Signal.T OBJECT
    METHODS
      init (x: Signal.T;
            mod: SignalControl.T;  (* the modulation means delay or
                                      prefetch, a value of 10 means that
                                      the current value of the output is
                                      equal to the value of input of 10
                                      steps in the future; -10 means a
                                      delay of 10 values *)
            min, max: INTEGER;   (* The caller must guarant that all values
                                    of mod are in the range [min,max], the
                                    phase modulator guarantees that it only
                                    looks 'max' values (plus values for
                                    interpolation) ahead.  This is
                                    important for feedback.) *)
            interpolation: Interpolation.T; ): T
            RAISES {Signal.End, Signal.Error, Thread.Alerted};
    END;

END ProcessPhaseModulation.
