MODULE ProcessPhaseModulation;

IMPORT ProcessDelay;
IMPORT Signal, SignalControl, Interpolation;
IMPORT Thread;

REVEAL
  T =
    Public BRANDED OBJECT
      x  : Signal.T;
      mod: SignalControl.T;
      ip : Interpolation.T;

      buffer: Signal.RefArray;   (* this buffer contains all samples around
                                    the current one, all data is hold twice
                                    in order to avoid a large copy every
                                    time the buffer end is reached *)
      (*****
        time 0:  0  1  2  3  0  1  2  3
        time 1:  4  1  2  3  4  1  2  3
        time 2:  4  5  2  3  4  5  2  3
        time 3:  4  5  6  3  4  5  6  3
        time 4:  4  5  6  7  4  5  6  7
        time 5:  8  5  6  7  8  5  6  7
        time 6:  8  9  6  7  8  9  6  7
        ...
      *)

      posGet: CARDINAL;          (* position in the buffer where to read
                                    the next sample *)
      pos: CARDINAL;             (* position of the current sample in the
                                    buffer *)
      min, max: INTEGER;
      number  : CARDINAL;        (* max-min+interpolationPad *)

    OVERRIDES
      init := Init;
      get  := Get;
      exit := Exit;
    END;


PROCEDURE Init (SELF         : T;
                x            : Signal.T;
                mod          : SignalControl.T;
                min, max     : INTEGER;
                interpolation: Interpolation.T; ): T
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    <* ASSERT min < max *>
    SELF.mod := mod;
    SELF.ip := interpolation;

    (* by the simple difference max-min we omit one value, this is read on
       the first invocation of get() *)
    SELF.number := max - min + Interpolation.NumOfValues[interpolation];

    SELF.min := min;
    SELF.max := max;

    SELF.posGet := 0;
    SELF.pos := -min;
    SELF.buffer := NEW(Signal.RefArray, 2 * SELF.number);

    CASE SELF.ip OF
    | Interpolation.T.Constant =>
    | Interpolation.T.Linear =>
    | Interpolation.T.Cubic => DEC(min);
    END;
    SELF.x := NEW(ProcessDelay.T).init(x, -min);

    WITH buffer = SELF.buffer^ DO
      FOR j := 0 TO SELF.number - 1 DO buffer[j] := SELF.x.get(); END;
    END;
    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.buffer := NIL;
    SELF.mod.exit();
    SELF.mod := NIL;
    SELF.x.exit();
    SELF.x := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    WITH buffer  = SELF.buffer^,
         posGet  = SELF.posGet,
         number  = SELF.number,
         mod     = SELF.mod.get(),
         modInt  = FLOOR(mod),
         modFrac = mod - FLOAT(modInt, LONGREAL),
         modPos  = SELF.pos + modInt              DO

      <* ASSERT 0.0D0 <= modFrac AND modFrac < 1.0D0 *>
      <* ASSERT SELF.min <= modInt AND modInt <= SELF.max *>
      <* ASSERT posGet < number *>

      WITH x = SELF.x.get() DO
        buffer[posGet] := x;
        buffer[posGet + number] := x;
      END;
      INC(posGet);
      INC(SELF.pos);

      (* When the end of the buffer is reached, switch back to the first
         half.  Since we always read to two positions, the lower half is
         already filled. *)
      IF posGet = number THEN SELF.pos := -SELF.min; posGet := 0; END;

      CASE SELF.ip OF
      | Interpolation.T.Constant => RETURN buffer[modPos];
      | Interpolation.T.Linear =>
          RETURN
            Interpolation.Linear(SUBARRAY(buffer, modPos, 2), modFrac);
      | Interpolation.T.Cubic =>
          RETURN Interpolation.Cubic(SUBARRAY(buffer, modPos, 4), modFrac);
      END;
    END;
  END Get;

BEGIN
END ProcessPhaseModulation.
