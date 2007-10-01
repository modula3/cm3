MODULE ProcessConcat;

IMPORT Signal;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
        x           : REF ARRAY OF Signal.T;
        currentInput: CARDINAL;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; READONLY x: ARRAY OF Signal.T; ): T =
  BEGIN
    SELF.x := NEW(REF ARRAY OF Signal.T, NUMBER(x));
    SELF.x^ := x;
    SELF.currentInput := 0;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    FOR j := FIRST(SELF.x^) TO LAST(SELF.x^) DO SELF.x[j].exit(); END;
    SELF.x := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    WITH j = SELF.currentInput DO
      <* ASSERT j < NUMBER(SELF.x^) *>
      LOOP
        TRY
          RETURN SELF.x[j].get();
        EXCEPT
        | Signal.End =>
            INC(j);
            IF j = NUMBER(SELF.x^) THEN RAISE Signal.End; END;
        END;
      END;
    END;
  END Get;

BEGIN
END ProcessConcat.
