GENERIC MODULE Signal(R,(*Signal,*)SignalRep);

REVEAL
  T = SignalRep.TPrivate BRANDED OBJECT
	OVERRIDES
	init      := Init;
	fromArray := FromArray;
	copy      := Copy;

	getFirst  := GetFirst;
	getLast   := GetLast;
	getNumber := GetNumber;

	upsample   := UpSample;
	downsample := DownSample;
	wrapCyclic := WrapCyclic;
	reverse    := Reverse;
	adjungate  := Adjungate;

    scale      := Scale;
    raise      := Raise;

	translateD := TranslateD;
    scaleD     := ScaleD;
    raiseD     := RaiseD;

    convolve   := Convolve;
    superpose  := Superpose;
  END;


PROCEDURE Init (SELF : T; first, last : IndexType) : T =
  BEGIN
	SELF.data  := NEW(REF ARRAY OF R.T, last-first);
	SELF.first := first;
	FOR j:=0 TO LAST(SELF.data^) DO
      SELF.data[j] := R.Zero;
	END;
	RETURN SELF;
  END Init;

PROCEDURE FromArray (SELF : T; READONLY arr : ARRAY OF R.T; first : IndexType) : T =
  BEGIN
	SELF.data  := NEW(REF ARRAY OF R.T, NUMBER(arr));
	SELF.data^ := arr;
	SELF.first := first;
	RETURN SELF;
  END FromArray;

PROCEDURE Copy (SELF : T) : T =
  VAR
	z := NEW(T);
  BEGIN
    z.data  := NEW(REF ARRAY OF R.T, NUMBER(SELF.data^));
    z.first := SELF.first;
	FOR j:=FIRST(SELF.data^) TO LAST(SELF.data^) DO
	  z.data[j] := SELF.data[j];
	END;
	RETURN z;
  END Copy;


PROCEDURE GetFirst (SELF : T) : IndexType =
  BEGIN
	RETURN SELF.first;
  END GetFirst;

PROCEDURE GetLast (SELF : T) : IndexType =
  BEGIN
	RETURN SELF.first + NUMBER(SELF.data^);
  END GetLast;

PROCEDURE GetNumber (SELF : T) : IndexType =
  BEGIN
	RETURN NUMBER(SELF.data^);
  END GetNumber;


PROCEDURE TranslateD (SELF : T; dist : IndexType) =
  BEGIN
	INC (SELF.first, dist);
  END TranslateD;

PROCEDURE UpSample (x : T; factor : IndexType) : T =
  VAR
	z := NEW(T).init(x.getFirst()*factor,(x.getLast()-1)*factor+1);
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
      z.data[i*factor] := x.data[i];
	END;
	RETURN z;
  END UpSample;

PROCEDURE DownSample (x : T; factor : IndexType) : T =
  VAR
	z := NEW(T).init (-((-x.getFirst()) DIV factor), (x.getLast()-1) DIV factor +1);
  BEGIN
	FOR i:=z.first TO z.first+LAST(z.data^) DO
      z.data[i-z.first] := x.data[i*factor-x.first];
	END;
	RETURN z;
  END DownSample;

PROCEDURE WrapCyclic (x : T; length : IndexType) : T =
  VAR
	z := NEW(T).init (0, length);
	j : IndexType;

  BEGIN
	j := x.first MOD length;
	FOR i:=0 TO LAST(z.data^) DO
      z.data[j] := R.Add (z.data[j], x.data[i]);
	  INC(j);
	  IF j>=length THEN
		j:=0;
      END;
	END;
	RETURN z;
  END WrapCyclic;

PROCEDURE Reverse (x : T) : T =
  VAR
	z := NEW(T);
  BEGIN
    z.data  := NEW(REF ARRAY OF R.T, NUMBER(x.data^));
    z.first := -(x.first + LAST(x.data^));
	FOR j:=FIRST(z.data^) TO LAST(z.data^) DO
	  z.data[j] := x.data[LAST(x.data^)+FIRST(z.data^)-j];
	END;
	RETURN z;
  END Reverse;

PROCEDURE Adjungate (x : T) : T =
  VAR
	z := NEW(T);
  BEGIN
    z.data  := NEW(REF ARRAY OF R.T, NUMBER(x.data^));
    z.first := -(x.first + LAST(x.data^));
	FOR j:=FIRST(z.data^) TO LAST(z.data^) DO
	  z.data[j] := R.Conj(x.data[LAST(x.data^)+FIRST(z.data^)-j]);
	END;
	RETURN z;
  END Adjungate;


PROCEDURE Scale (x : T; factor : R.T) : T =
  VAR
    z := NEW(T);
  BEGIN
    z.data  := NEW(REF ARRAY OF R.T, NUMBER(x.data^));
    z.first := x.first;
	FOR i:=0 TO LAST(x.data^) DO
      z.data[i] := R.Mul (x.data[i], factor);
	END;
    RETURN z;
  END Scale;

PROCEDURE ScaleD (x : T; factor : R.T) =
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
      x.data[i] := R.Mul (x.data[i], factor);
	END;
  END ScaleD;

PROCEDURE Raise (x : T; offset : R.T) : T =
  VAR
    z := NEW(T);
  BEGIN
    z.data  := NEW(REF ARRAY OF R.T, NUMBER(x.data^));
    z.first := x.first;
	FOR i:=0 TO LAST(x.data^) DO
      z.data[i] := R.Add (x.data[i], offset);
	END;
    RETURN z;
  END Raise;

PROCEDURE RaiseD (x : T; offset : R.T) =
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
      x.data[i] := R.Add (x.data[i], offset);
	END;
  END RaiseD;


PROCEDURE Convolve (x : T; y : T) : T =
  VAR
	z := NEW(T).init(x.getFirst()+y.getFirst(),x.getLast()+y.getLast()-1);
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
	  FOR j:=0 TO LAST(y.data^) DO
		z.data[i+j] := R.Add(z.data[i+j], R.Mul(x.data[i], y.data[j]));
	  END;
	END;
    RETURN z;
  END Convolve;

PROCEDURE Superpose (x : T; y : T) : T =
  VAR
	z := NEW(T).init(MIN(x.getFirst(),y.getFirst()),MAX(x.getLast(),y.getLast()));
	j : IndexType;

  BEGIN
	j := x.getFirst()-z.getFirst();
	FOR i:=0 TO LAST(x.data^) DO
	  z.data[i+j] := R.Add (z.data[i+j], x.data[i]);
	END;
	j := y.getFirst()-z.getFirst();
	FOR i:=0 TO LAST(y.data^) DO
	  z.data[i+j] := R.Add (z.data[i+j], y.data[i]);
	END;
    RETURN z;
  END Superpose;


BEGIN
END Signal.
