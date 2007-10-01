MODULE ProcessControlLinear;

IMPORT Signal, SignalControl;
IMPORT Thread;

PROCEDURE Do (READONLY x: Signal.Array; center, depth: LONGREAL; ):
  SignalControl.RefArray =
  VAR y := NEW(SignalControl.RefArray, NUMBER(x));
  BEGIN
    FOR j := FIRST(x) TO LAST(x) DO y[j] := center + depth * x[j]; END;
    RETURN y;
  END Do;


REVEAL
  T = Public BRANDED OBJECT
        x            : Signal.T;
        center, depth: LONGREAL;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;

PROCEDURE Init (SELF: T; x: Signal.T; center, depth: LONGREAL; ): T =
  BEGIN
    SELF.x := x;
    SELF.center := center;
    SELF.depth := depth;
    RETURN SELF;
  END Init;

PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    RETURN SELF.center + SELF.depth * SELF.x.get();
  END Get;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
  END Exit;

BEGIN
END ProcessControlLinear.
