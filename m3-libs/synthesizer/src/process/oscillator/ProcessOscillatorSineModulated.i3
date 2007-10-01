INTERFACE ProcessOscillatorSineModulated;

IMPORT Signal, Thread;
PROCEDURE Do (READONLY freq: Signal.Array;
              phase := 0.0D0; (* beginning phase of the wave; e.g.  0.25D0
                                 means a sine starting at 90 degree (i.e.
                                 with value 1, that is a cosine) *) ):
  Signal.RefArray;

TYPE
  T = Signal.T OBJECT
        phase: LONGREAL;
        freq : Signal.T;
      METHODS
        init (freq: Signal.T; phase := 0.0D0; ): T := Init;
      OVERRIDES
        exit := Exit;
        get  := Get;
      END;

PROCEDURE Init (SELF: T; freq: Signal.T; phase: LONGREAL; ): T;

PROCEDURE Exit (SELF: T; );

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted};

END ProcessOscillatorSineModulated.
