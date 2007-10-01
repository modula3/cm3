INTERFACE ProcessComplexLowPass;
(* author: thielema *)

IMPORT Signal, SignalControl, ProcessMultiOutput;

(* This is a lowpass of first order with complex valued feedback.  This
   allows resonance.  It produces a complex valued output, whose absolute
   value can be used for frequency amplitude detection. *)

TYPE
  Part = {Real, Imaginary};

  T <: Public;
  Public = ProcessMultiOutput.T OBJECT
           METHODS
             init (         x         : Signal.T;
                            freq, reso: SignalControl.T;
                   READONLY outputs   : ARRAY OF Part;   ): T;
           END;


END ProcessComplexLowPass.
