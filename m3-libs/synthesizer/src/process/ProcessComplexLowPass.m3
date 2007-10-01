MODULE ProcessComplexLowPass;

IMPORT Signal, SignalControl;
IMPORT Math;

IMPORT Thread;
(* IMPORT IO, Fmt; *)


REVEAL
  T = Public BRANDED OBJECT
        x         : Signal.T;
        freq, reso: SignalControl.T;
        y         : REF ARRAY OF LONGREAL;
      OVERRIDES
        init := Init;
        exit := Exit;
        get  := Get;
      END;

PROCEDURE Init (         SELF      : T;
                         x         : Signal.T;
                         freq, reso: SignalControl.T;
                READONLY outputs   : ARRAY OF Part;   ): T =
  BEGIN
    WITH outputsNumeric = NEW(REF ARRAY OF CARDINAL, NUMBER(outputs))^ DO
      FOR j := FIRST(outputs) TO LAST(outputs) DO
        outputsNumeric[j] := ORD(outputs[j]);
      END;
      SELF.createChannels(NUMBER(Part), outputsNumeric);
    END;

    SELF.y := NEW(REF ARRAY OF LONGREAL, NUMBER(Part));
    SELF.y^ := ARRAY OF LONGREAL{0.0D0, 0.0D0};

    SELF.x := x;
    SELF.freq := freq;
    SELF.reso := reso;

    RETURN SELF;
  END Init;

PROCEDURE Exit (SELF: T; ) =
  BEGIN
    SELF.x.exit();
    SELF.freq.exit();
    SELF.reso.exit();
  END Exit;


PROCEDURE Get (SELF: T; ): REF ARRAY OF LONGREAL
  RAISES {Signal.End, Signal.Error, Thread.Alerted} =
  VAR
    f   := 2.0D0 * Math.Pi * SELF.freq.get();
    cr  := Math.cos(f);
    ci  := Math.sin(f);
    q   := SELF.reso.get();
    q2  := q * q;
    q2r := q2 - cr;
    q21 := q2 - 1.0D0;
    (** probably numerically less stable for large q
       p := q2r / q21;
       c := p - Math.sqrt(p * p - 1.0D0);
    *)
    (** division by zero when q = 1
       c := (q2r - Math.sqrt((q2r + q21) * (1.0D0 - cr))) / q21;
    *)
    c := q21 / (q2r + Math.sqrt((q2r + q21) * (1.0D0 - cr)));

  BEGIN
    cr := cr * c;
    ci := ci * c;
    (* IO.Put(Fmt.LongReal(Math.sqrt((1.0D0 - cr) * (1.0D0 - cr) + ci * ci)
       / (1.0D0 - c)) & "\n"); *)
    WITH yr  = SELF.y[ORD(Part.Real)],
         yi  = SELF.y[ORD(Part.Imaginary)],
         x   = SELF.x.get(),
         yr1 = cr * (yr - x) - ci * yi + x,
         yi1 = ci * (yr - x) + cr * yi      DO
      yr := yr1;
      yi := yi1;
    END;
    RETURN SELF.y;
  END Get;

BEGIN
END ProcessComplexLowPass.
