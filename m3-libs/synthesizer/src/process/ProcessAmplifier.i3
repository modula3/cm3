
INTERFACE ProcessAmplifier;

IMPORT Signal, SignalControl;

PROCEDURE Do
  (READONLY x: Signal.Array; READONLY envelope: SignalControl.Array; ):
  Signal.RefArray;


TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x: Signal.T; envelope: SignalControl.T; ): T;
           END;


END ProcessAmplifier.
