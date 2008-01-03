INTERFACE ProcessOscillatorModulated;
(* author: thielema *)

FROM ProcessOscillator IMPORT WaveFunc;

IMPORT Signal;


TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (wave: WaveFunc; freq: Signal.T; phase := 0.0D0; ): T;
           END;


END ProcessOscillatorModulated.
