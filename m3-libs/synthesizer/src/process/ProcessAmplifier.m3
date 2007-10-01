MODULE ProcessAmplifier;

IMPORT Signal, SignalControl, Thread;



PROCEDURE Do (READONLY a, b: Signal.Array; ): Signal.RefArray =
  VAR x := NEW(Signal.RefArray, NUMBER(a));
  BEGIN
    <* ASSERT NUMBER(a) = NUMBER(b) *>
    FOR j := FIRST(x^) TO LAST(x^) DO x[j] := a[j] * b[j]; END;
    RETURN x;
  END Do;



REVEAL
  T = Public BRANDED OBJECT
        x, y: Signal.T;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; x: Signal.T; envelope: SignalControl.T; ): T =
  BEGIN
    SELF.x := x;
    SELF.y := envelope;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.x := NIL;
    SELF.y.exit();
    SELF.y := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    RETURN SELF.x.get() * SELF.y.get();
  END Get;

BEGIN
END ProcessAmplifier.
