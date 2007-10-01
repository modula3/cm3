MODULE ProcessMixer;

IMPORT Signal, Thread;

PROCEDURE Do (READONLY x, y: Signal.Array; ): Signal.RefArray =
  VAR z := NEW(Signal.RefArray, NUMBER(x));
  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(y) *>
    FOR j := FIRST(z^) TO LAST(z^) DO z[j] := x[j] + y[j]; END;

    RETURN z;
  END Do;


REVEAL
  T = Public BRANDED OBJECT
        x, y: Signal.T;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;

PROCEDURE Init (SELF: T; x, y: Signal.T; ): T =
  BEGIN
    SELF.x := x;
    SELF.y := y;

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
    RETURN SELF.x.get() + SELF.y.get();
  END Get;

BEGIN
END ProcessMixer.
