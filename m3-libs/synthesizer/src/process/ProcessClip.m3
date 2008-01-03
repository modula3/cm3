MODULE ProcessClip;

IMPORT Signal;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
        x       : Signal.T;
        duration: CARDINAL;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; x: Signal.T; duration: INTEGER; ): T =
  BEGIN
    SELF.x := x;
    SELF.duration := duration;

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
    IF SELF.duration = 0 THEN
      RAISE Signal.End;
    ELSE
      DEC(SELF.duration);
      RETURN SELF.x.get();
    END;
  END Get;

BEGIN
END ProcessClip.
