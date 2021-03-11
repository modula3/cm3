MODULE PMPTimer;
IMPORT Thread, BreakHere;

TYPE
  Closure = Thread.Closure OBJECT
    initialDelay, repeatDelay: LONGREAL;
  OVERRIDES
    apply := Daemon;
  END;
PROCEDURE Daemon(self: Closure): REFANY =
  BEGIN
    Thread.Pause(self.initialDelay);
    BreakHere.Please();
    LOOP
      Thread.Pause(self.repeatDelay);
    END;
  END Daemon;


PROCEDURE Start(initialDelay := 10.0D0; repeatDelay := 1.0D0) =
  BEGIN
    EVAL Thread.Fork(NEW(Closure,
                         initialDelay := initialDelay,
                         repeatDelay := repeatDelay));
  END Start;

BEGIN
END PMPTimer.
