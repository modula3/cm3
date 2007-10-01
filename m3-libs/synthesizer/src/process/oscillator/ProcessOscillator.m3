MODULE ProcessOscillator;

REVEAL
  T = Public BRANDED OBJECT
        wave       : WaveFunc;
        phase, freq: LONGREAL;
      OVERRIDES
        init := Init;
        get  := Get;
      END;


PROCEDURE Wrap (x: LONGREAL; ): LONGREAL =
  BEGIN
    WHILE x >= 1.0D0 DO x := x - 1.0D0; END;
    WHILE x < 0.0D0 DO x := x + 1.0D0; END;
    RETURN x;
  END Wrap;


PROCEDURE Init (SELF: T; wave: WaveFunc; freq, phase: LONGREAL; ): T =
  BEGIN
    SELF.wave := wave;
    SELF.phase := Wrap(phase);
    SELF.freq := Wrap(freq);
    RETURN SELF;
  END Init;

PROCEDURE Get (SELF: T; ): LONGREAL =
  VAR x: LONGREAL;
  BEGIN
    WITH phase = SELF.phase DO
      x := SELF.wave(phase);
      phase := phase + SELF.freq;
      IF phase >= 1.0D0 THEN phase := phase - 1.0D0; END;
      RETURN x;
    END;
  END Get;

BEGIN
END ProcessOscillator.
