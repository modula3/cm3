MODULE ProcessDelay;

IMPORT Signal;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
        x       : Signal.T;
        numZeros: CARDINAL;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; x: Signal.T; delay: INTEGER; ): T
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    SELF.x := x;
    WHILE delay < 0 DO EVAL x.get(); INC(delay); END;
    SELF.numZeros := delay;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.x := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    IF SELF.numZeros > 0 THEN
      DEC(SELF.numZeros);
      RETURN 0.0D0;
    ELSE
      RETURN SELF.x.get();
    END;
  END Get;

BEGIN
END ProcessDelay.
