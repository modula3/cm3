MODULE ProcessStateVariableFilter;

IMPORT Signal, SignalControl;
IMPORT Math;

IMPORT Thread;


PROCEDURE Do
  (READONLY x: Signal.Array; READONLY freq, reso: SignalControl.Array; ):
  ARRAY Type OF Signal.RefArray =
  VAR
    y := ARRAY Type OF
           Signal.RefArray{NEW(Signal.RefArray, NUMBER(x)),
                           NEW(Signal.RefArray, NUMBER(x)),
                           NEW(Signal.RefArray, NUMBER(x))};
    yl, yb, yh: LONGREAL := 0.0D0;
  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(freq) *>
    <* ASSERT NUMBER(x) = NUMBER(reso) *>
    FOR j := FIRST(x) TO LAST(x) DO
      WITH f = 2.0D0 * Math.sin(Math.Pi * freq[j]),
           q = 2.0D0 * reso[j]                      DO
        yl := f * yb + yl;
        yb := f * yh + yb;
        yh := x[j] - yl - q * yb;
        y[Type.Highpass, j] := yh;
        y[Type.Bandpass, j] := yb;
        y[Type.Lowpass, j] := yl;
      END;
    END;
    RETURN y;
  END Do;

REVEAL
  T = Public BRANDED OBJECT
        x         : Signal.T;
        freq, reso: SignalControl.T;
        y         : REF ARRAY OF LONGREAL;
      OVERRIDES
        init := Init;
        exit := Exit;
        get  := Get;
      END;

PROCEDURE Init (         SELF      : T;
                         x         : Signal.T;
                         freq, reso: SignalControl.T;
                READONLY outputs   : ARRAY OF Type;   ): T =
  BEGIN
    WITH outputsNumeric = NEW(REF ARRAY OF CARDINAL, NUMBER(outputs))^ DO
      FOR j := FIRST(outputs) TO LAST(outputs) DO
        outputsNumeric[j] := ORD(outputs[j]);
      END;
      SELF.createChannels(NUMBER(Type), outputsNumeric);
    END;

    SELF.y := NEW(REF ARRAY OF LONGREAL, NUMBER(Type));
    SELF.y^ := ARRAY OF LONGREAL{0.0D0, 0.0D0, 0.0D0};

    SELF.x := x;
    SELF.freq := freq;
    SELF.reso := reso;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.freq.exit();
    SELF.reso.exit();
  END Exit;


PROCEDURE Get (SELF: T; ): REF ARRAY OF LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR
    f := 2.0D0 * Math.sin(Math.Pi * SELF.freq.get());
    q := 2.0D0 * SELF.reso.get();
  BEGIN
    WITH yl = SELF.y[ORD(Type.Lowpass)],
         yb = SELF.y[ORD(Type.Bandpass)],
         yh = SELF.y[ORD(Type.Highpass)]  DO
      yl := f * yb + yl;
      yb := f * yh + yb;
      yh := SELF.x.get() - yl - q * yb;
    END;
    RETURN SELF.y;
  END Get;

BEGIN
END ProcessStateVariableFilter.
