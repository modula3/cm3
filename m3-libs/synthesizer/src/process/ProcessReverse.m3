MODULE ProcessReverse;

IMPORT Signal, LongRealSeq, Thread;

PROCEDURE Do (READONLY x: Signal.Array; ): Signal.RefArray =
  VAR z := NEW(Signal.RefArray, NUMBER(x));
  BEGIN
    FOR j := FIRST(z^) TO LAST(z^) DO z[j] := x[LAST(x) - j]; END;
    RETURN z;
  END Do;


REVEAL
  T = Public BRANDED OBJECT
        x: LongRealSeq.T;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; x: Signal.T; ): T
  RAISES {Signal.Error, Thread.Alerted} =
  BEGIN
    SELF.x := NEW(LongRealSeq.T).init(sizeHint := 100);
    TRY
      TRY LOOP SELF.x.addhi(x.get()); END; EXCEPT | Signal.End => END;
    FINALLY
      x.exit();
    END;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL =
  BEGIN
    RETURN SELF.x.remhi();
  END Get;

BEGIN
END ProcessReverse.
