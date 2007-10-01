MODULE ProcessFrequencyModulation;

IMPORT Signal, SignalControl, Interpolation;
IMPORT Thread;

REVEAL
  T = Public BRANDED OBJECT
        x  : Signal.T;
        mod: SignalControl.T;
        ip : Interpolation.T;

        buffer: ARRAY [0 .. 63] OF LONGREAL;
        numValid: CARDINAL;      (* number of numValid samples in buffer;
                                    numValid - pos is constant but depends
                                    on the interpolation type *)
        pos: CARDINAL;
        frac: LONGREAL;          (* 0<=frac AND frac<1, pos and frac
                                    constitute the fractional position
                                    within the buffer *)
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF         : T;
                x            : Signal.T;
                mod          : SignalControl.T;
                interpolation: Interpolation.T; ): T
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  BEGIN
    SELF.x := x;
    SELF.mod := mod;
    SELF.ip := interpolation;
    SELF.numValid := Interpolation.NumOfValues[interpolation];
    SELF.frac := 0.0D0;
    SELF.pos := 0;
    WITH buffer = SELF.buffer DO
      CASE SELF.ip OF
      | Interpolation.T.Constant => buffer[0] := x.get();
      | Interpolation.T.Linear =>
          buffer[0] := x.get();
          buffer[1] := x.get();
      | Interpolation.T.Cubic =>
          buffer[0] := 0.0D0;
          buffer[1] := x.get();
          buffer[2] := x.get();
          buffer[3] := x.get();
      END;
    END;
    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.mod.exit();
    SELF.x.exit();
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR y: LONGREAL;
  BEGIN
    WITH pos      = SELF.pos,
         frac     = SELF.frac,
         buffer   = SELF.buffer,
         numValid = SELF.numValid,
         inc      = SELF.mod.get() DO

      <* ASSERT 0.0D0 <= frac AND frac < 1.0D0 *>
      <* ASSERT inc >= 0.0D0 *>

      CASE SELF.ip OF
      | Interpolation.T.Constant => y := buffer[pos];
      | Interpolation.T.Linear =>
          y := Interpolation.Linear(SUBARRAY(buffer, pos, 2), frac);
      | Interpolation.T.Cubic =>
          y := Interpolation.Cubic(SUBARRAY(buffer, pos, 4), frac);
      END;

      frac := frac + inc;
      WHILE frac >= 1.0D0 DO
        frac := frac - 1.0D0;
        INC(pos);
        buffer[numValid] := SELF.x.get();
        INC(numValid);
        IF numValid = NUMBER(buffer) THEN
          (* If the buffer is full: start from the beginning, again *)
          WITH num = Interpolation.NumOfValues[SELF.ip] DO
            SUBARRAY(buffer, 0, num) :=
              SUBARRAY(buffer, NUMBER(buffer) - num, num);
            pos := 0;
            numValid := num;
          END;
        END;
      END;
    END;
    RETURN y;
  END Get;

BEGIN
END ProcessFrequencyModulation.
