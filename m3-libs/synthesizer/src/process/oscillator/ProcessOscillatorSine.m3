MODULE ProcessOscillatorSine;

IMPORT Signal;
IMPORT Math;

PROCEDURE Do (length: CARDINAL; freq, phase: LONGREAL; ): Signal.RefArray =
  VAR x := NEW(Signal.RefArray, length);
  BEGIN
    phase := phase * Math.Pi * 2.0D0;
    freq := freq * Math.Pi * 2.0D0;
    FOR j := FIRST(x^) TO LAST(x^) DO
      x[j] := Math.sin(phase);
      phase := phase + freq;
    END;
    RETURN x;
  END Do;


BEGIN
END ProcessOscillatorSine.
