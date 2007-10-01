INTERFACE ProcessOscillator;
(* author: thielema *)

IMPORT Signal;


TYPE
  WaveFunc = PROCEDURE (phase: LONGREAL; ): LONGREAL;

  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (wave: WaveFunc;  (* A map from the phase between 0 and 1
                                       to the wave value. *)
                   freq : LONGREAL;
                   phase             := 0.0D0; ): T;
           END;

PROCEDURE Wrap (x: LONGREAL; ): LONGREAL;

END ProcessOscillator.
