MODULE ProcessOscillatorSineModulated;

IMPORT Signal, Math, Thread;

PROCEDURE Do (READONLY freq: Signal.Array;
              phase := 0.0D0; (* beginning phase of the wave; e.g.  0.25D0
                                 means a sine starting at 90 degree (i.e.
                                 with value 1, that is a cosine) *) ):
  Signal.RefArray =

  VAR z := NEW(Signal.RefArray, NUMBER(freq));


  BEGIN
    phase := phase * Math.Pi * 2.0D0;
    FOR j := FIRST(z^) TO LAST(z^) DO
      z[j] := Math.sin(phase);
      phase := phase + freq[j] * Math.Pi * 2.0D0;
    END;
    RETURN z;
  END Do;

PROCEDURE Init (SELF: T; freq: Signal.T; phase: LONGREAL; ): T =
  BEGIN
    SELF.phase := phase * Math.Pi * 2.0D0;
    SELF.freq := freq;
    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.freq.exit();
    SELF.freq := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR z := Math.sin(SELF.phase);
  BEGIN
    SELF.phase := SELF.phase + SELF.freq.get() * Math.Pi * 2.0D0;
    RETURN z;
  END Get;

BEGIN
END ProcessOscillatorSineModulated.
