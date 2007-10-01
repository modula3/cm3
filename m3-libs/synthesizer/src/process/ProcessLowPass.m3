MODULE ProcessLowPass;

IMPORT Signal, SignalControl, Math;
IMPORT Thread;


PROCEDURE Do
  (READONLY x: Signal.Array; READONLY freq: SignalControl.Array; ):
  Signal.RefArray =
  VAR
    y           := NEW(Signal.RefArray, NUMBER(x));
    c: LONGREAL;
    z: LONGREAL := 0.0D0;

  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(freq) *>

    FOR j := FIRST(x) TO LAST(x) DO
      c := Math.exp(-2.0D0 * Math.Pi * freq[j]);
      z := z * c + x[j] * (1.0D0 - c);
      y[j] := z;
    END;

    RETURN y;
  END Do;


REVEAL
  T = Public BRANDED OBJECT
        x   : Signal.T;
        freq: SignalControl.T;
        y   : LONGREAL;
      OVERRIDES
        init := Init;
        get  := Get;
        exit := Exit;
      END;


PROCEDURE Init (SELF: T; x: Signal.T; freq: SignalControl.T; ): T =
  BEGIN
    SELF.x := x;
    SELF.freq := freq;
    SELF.y := 0.0D0;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.x := NIL;
    SELF.freq.exit();
    SELF.freq := NIL;
  END Exit;


PROCEDURE Get (SELF: T; ): LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =

  VAR
    (* two alternative computations which are asymptotically equivalent for
       low frequencies *)

    (* with this definition we get the same halflife of the impulse
       response as with a RC lowpass of first order *)
    (* c := Math.exp(-2.0D0 * Math.Pi * SELF.freq.get()); *)

    (* from DAFx book *)
    k := Math.tan(Math.Pi * SELF.freq.get());
    c := (1.0D0 - k) / (1.0D0 + k);

  (* Also a reasonable approximation: k := Math.Pi * SELF.freq.get() *)

  BEGIN
    SELF.y := SELF.y * c + SELF.x.get() * (1.0D0 - c);
    RETURN SELF.y;
  END Get;


BEGIN
END ProcessLowPass.
