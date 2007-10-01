MODULE ProcessOscillatorModulated;

FROM ProcessOscillator IMPORT WaveFunc, Wrap;

IMPORT Signal, Thread;


REVEAL
  T = Public BRANDED OBJECT
        wave : WaveFunc;
        freq : Signal.T;
        phase: LONGREAL;
      OVERRIDES
        init := Init;
        exit := Exit;
        get  := Get;
      END;



PROCEDURE Init
  (SELF: T; wave: WaveFunc; freq: Signal.T; phase: LONGREAL; ): T =
  BEGIN
    SELF.wave := wave;
    SELF.freq := freq;
    SELF.phase := Wrap(phase);
    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.freq.exit();
    SELF.freq := NIL;
  END Exit;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    WITH phase = SELF.phase,
         x     = SELF.wave(phase) DO
      phase := Wrap(phase + SELF.freq.get());
      RETURN x;
    END;
  END Get;

BEGIN
END ProcessOscillatorModulated.
