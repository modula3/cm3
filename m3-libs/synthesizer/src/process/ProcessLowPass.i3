INTERFACE ProcessLowPass;
(* author: sbeckers*)

IMPORT Signal, SignalControl;



PROCEDURE Do (READONLY x   : Signal.Array;         (* input *)
              READONLY freq: SignalControl.Array;  (* frequency *)):
  Signal.RefArray;               (* output *)


TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x: Signal.T; freq: SignalControl.T; ): T;
           END;


END ProcessLowPass.
