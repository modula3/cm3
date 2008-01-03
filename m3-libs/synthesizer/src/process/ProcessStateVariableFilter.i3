INTERFACE ProcessStateVariableFilter;
(* author: thielema *)

IMPORT Signal, SignalControl, ProcessMultiOutput;

(* The state variable filter is a simultaneous highpass, bandpass and
   lowpass filter. *)

TYPE Type = {Highpass, Bandpass, Lowpass};

PROCEDURE Do
  (READONLY x: Signal.Array;     (* input signal *)
   READONLY freq, reso: SignalControl.Array;  (* control of the resonance
                                                 frequency and the
                                                 resonance *)):
  ARRAY Type OF Signal.RefArray;


TYPE
  T <: Public;
  Public = ProcessMultiOutput.T OBJECT
           METHODS
             init (         x         : Signal.T;
                            freq, reso: SignalControl.T;
                   READONLY outputs   : ARRAY OF Type;   ): T;
           END;


END ProcessStateVariableFilter.
