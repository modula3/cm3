GENERIC MODULE Signal(R, SignalRep, V, P);

REVEAL
  T = SignalRep.TPrivate BRANDED OBJECT
      OVERRIDES
        init      := Init;
        initFL    := InitFL;
        fromArray := FromArray;
        copy      := Copy;

        getFirst  := GetFirst;
        getLast   := GetLast;
        getNumber := GetNumber;
        getData   := GetData;

        sum := Sum;

        upsample   := UpSample;
        downsample := DownSample;
        wrapCyclic := WrapCyclic;
        slice      := Slice;
        interleave := Interleave;
        reverse    := Reverse;
        adjoint    := Adjoint;

        translate := Translate;
        scale     := Scale;
        negate    := Neg;
        raise     := Raise;

        translateD := TranslateD;
        scaleD     := ScaleD;

        alternate := Alternate;

        convolve  := Mul;
        superpose := Add;
      END;


PROCEDURE Init (SELF: T; first, number: IndexType): T =
  BEGIN
    SELF.data := NEW(V.T, number);
    SELF.first := first;
    FOR j := 0 TO LAST(SELF.data^) DO SELF.data[j] := R.Zero; END;
    RETURN SELF;
  END Init;

PROCEDURE InitFL (SELF: T; first, last: IndexType): T =
  BEGIN
    RETURN Init(SELF, first, last - first + 1);
  END InitFL;

PROCEDURE FromArray (SELF: T; READONLY arr: ARRAY OF R.T; first: IndexType):
  T =
  BEGIN
    SELF.data := NEW(V.T, NUMBER(arr));
    SELF.data^ := arr;
    SELF.first := first;
    RETURN SELF;
  END FromArray;

PROCEDURE Copy (SELF: T): T =
  VAR z := NEW(T);
  BEGIN
    z.data := NEW(V.T, NUMBER(SELF.data^));
    z.first := SELF.first;
    z.data^ := SELF.data^;
    RETURN z;
  END Copy;


PROCEDURE GetFirst (SELF: T): IndexType =
  BEGIN
    RETURN SELF.first;
  END GetFirst;

PROCEDURE GetLast (SELF: T): IndexType =
  BEGIN
    RETURN SELF.first + LAST(SELF.data^);
  END GetLast;

PROCEDURE GetNumber (SELF: T): IndexType =
  BEGIN
    RETURN NUMBER(SELF.data^);
  END GetNumber;

PROCEDURE GetData (SELF: T): P.T =
  BEGIN
    RETURN SELF.data;
  END GetData;


PROCEDURE Sum (SELF: T): R.T =
  BEGIN
    RETURN V.Sum(SELF.data^);
  END Sum;


PROCEDURE Translate (SELF: T; dist: IndexType): T =
  BEGIN
    RETURN NEW(T, data := SELF.data, first := SELF.first + dist);
  END Translate;

PROCEDURE TranslateD (SELF: T; dist: IndexType) =
  BEGIN
    INC(SELF.first, dist);
  END TranslateD;

PROCEDURE UpSample (x: T; factor: IndexType): T =
  VAR z := NEW(T).init(x.first * factor, LAST(x.data^) * factor + 1);
  BEGIN
    FOR i := 0 TO LAST(x.data^) DO z.data[i * factor] := x.data[i]; END;
    RETURN z;
  END UpSample;

PROCEDURE DownSample (x: T; factor: IndexType): T =
  VAR
    z               := NEW(T);
    last: IndexType := x.getLast() DIV factor;
  BEGIN
    z.first := -((-x.first) DIV factor);
    (*
      z.data  := NEW(V.T, (LAST(x.data^)-((-x.first) MOD NUMBER(x.data^))) DIV factor);
    *)
    z.data := NEW(V.T, last - z.first + 1);
    FOR i := z.first TO z.getLast() DO
      z.data[i - z.first] := x.data[i * factor - x.first];
    END;
    RETURN z;
  END DownSample;

PROCEDURE WrapCyclic (x: T; length: IndexType): T =
  VAR
    z            := NEW(T).init(0, length);
    j: IndexType;

  BEGIN
    j := x.first MOD length;
    FOR i := 0 TO LAST(x.data^) DO
      z.data[j] := R.Add(z.data[j], x.data[i]);
      INC(j);
      IF j >= length THEN j := 0; END;
      (*
      j:=(j+1) MOD length;
      *)
    END;
    RETURN z;
  END WrapCyclic;

PROCEDURE Slice (x: T; num: IndexType): REF ARRAY OF T =
  VAR slice := NEW(REF ARRAY OF T, num);
  BEGIN
    FOR j := 0 TO LAST(slice^) DO
      slice[j] := x.translate(-j).downsample(num);
    END;
    RETURN slice;
  END Slice;

PROCEDURE Interleave (x: T; READONLY slice: ARRAY OF T): T =
  VAR
    first := NUMBER(slice) * slice[0].getFirst();
    last  := NUMBER(slice) * slice[0].getLast();
  BEGIN
    FOR j := 1 TO LAST(slice) DO
      first := MIN(first, j + NUMBER(slice) * slice[j].getFirst());
      last := MAX(last, j + NUMBER(slice) * slice[j].getLast());
    END;
    EVAL x.init(first, last + 1 - first);
    FOR j := 0 TO LAST(slice) DO
      VAR
        l    := j + NUMBER(slice) * slice[j].first - first;
        data := slice[j].data;
      BEGIN
        FOR k := 0 TO LAST(data^) DO
          x.data[l] := data[k];
          INC(l, NUMBER(slice));
        END;
      END;
    END;
    RETURN x;
  END Interleave;

PROCEDURE Reverse (x: T): T =
  VAR z := NEW(T);
  BEGIN
    z.data := NEW(V.T, NUMBER(x.data^));
    z.first := -(x.first + LAST(x.data^));
    FOR j := FIRST(z.data^) TO LAST(z.data^) DO
      z.data[j] := x.data[LAST(x.data^) + FIRST(z.data^) - j];
    END;
    RETURN z;
  END Reverse;

PROCEDURE Adjoint (x: T): T =
  VAR z := NEW(T);
  BEGIN
    z.data := NEW(V.T, NUMBER(x.data^));
    z.first := -GetLast(x);
    FOR j := FIRST(z.data^) TO LAST(z.data^) DO
      z.data[j] := R.Conj(x.data[LAST(x.data^) + FIRST(z.data^) - j]);
    END;
    RETURN z;
  END Adjoint;


PROCEDURE Scale (x: T; factor: R.T): T =
  VAR z := NEW(T);
  BEGIN
    z.data := NEW(V.T, NUMBER(x.data^));
    z.first := x.first;
    FOR i := 0 TO LAST(x.data^) DO
      z.data[i] := R.Mul(x.data[i], factor);
    END;
    RETURN z;
  END Scale;

PROCEDURE Neg (x: T): T =
  VAR z := NEW(T);
  BEGIN
    z.data := NEW(V.T, NUMBER(x.data^));
    z.first := x.first;
    FOR i := 0 TO LAST(x.data^) DO z.data[i] := R.Neg(x.data[i]); END;
    RETURN z;
  END Neg;

PROCEDURE ScaleD (x: T; factor: R.T) =
  BEGIN
    FOR i := 0 TO LAST(x.data^) DO
      x.data[i] := R.Mul(x.data[i], factor);
    END;
  END ScaleD;

PROCEDURE Raise (x: T; offset: R.T; first, number: IndexType): T =
  VAR
    zFirst := MIN(x.first, first);
    zLast  := MAX(x.first + NUMBER(x.data^), first + number) - 1;
    z      := NEW(T).initFL(zFirst, zLast);
  BEGIN
    WITH zdata = SUBARRAY(z.data^, x.first - z.first, NUMBER(x.data^)) DO
      zdata := x.data^;
    END;
    WITH zdata = SUBARRAY(z.data^, first - z.first, number) DO
      FOR i := 0 TO number - 1 DO zdata[i] := R.Add(zdata[i], offset); END;
    END;
    RETURN z;
  END Raise;


PROCEDURE Alternate (x: T): T =
  VAR
    z    := NEW(T).init(x.first, NUMBER(x.data^));
    sign := R.One;
  BEGIN
    IF x.first MOD 2 # 0 THEN sign := R.Neg(R.One); END;
    FOR i := 0 TO LAST(x.data^) DO
      z.data[i] := R.Mul(sign, x.data[i]);
      sign := R.Neg(sign);
    END;
    RETURN z;
  END Alternate;

<*UNUSED*>
PROCEDURE AlternateBool (x: T): T =
  VAR
    z    := NEW(T).init(x.first, NUMBER(x.data^));
    sign := x.first MOD 2 # 0;
  BEGIN
    FOR i := 0 TO LAST(x.data^) DO
      IF sign THEN
        z.data[i] := R.Neg(x.data[i]);
      ELSE
        z.data[i] := x.data[i];
      END;
      sign := NOT sign;
    END;
    RETURN z;
  END AlternateBool;

PROCEDURE Mul (x: T; y: T): T =
  VAR z := NEW(T).init(x.first + y.first, NUMBER(x.data^) + LAST(y.data^));
  BEGIN
    FOR i := 0 TO LAST(x.data^) DO
      WITH zdata = SUBARRAY(z.data^, i, NUMBER(y.data^)) DO
        FOR j := 0 TO LAST(y.data^) DO
          zdata[j] := R.Add(zdata[j], R.Mul(x.data[i], y.data[j]));
        END;
      END;
    END;
    RETURN z;
  END Mul;

PROCEDURE Add (x: T; y: T): T =
  VAR
    first := MIN(x.getFirst(), y.getFirst());
    last  := MAX(x.getLast(), y.getLast());
    z     := NEW(T).initFL(first, last);

  BEGIN
    WITH zdata = SUBARRAY(z.data^, x.first - z.first, NUMBER(x.data^)) DO
      FOR i := 0 TO LAST(x.data^) DO
        zdata[i] := R.Add(zdata[i], x.data[i]);
      END;
    END;
    WITH zdata = SUBARRAY(z.data^, y.first - z.first, NUMBER(y.data^)) DO
      FOR i := 0 TO LAST(y.data^) DO
        zdata[i] := R.Add(zdata[i], y.data[i]);
      END;
    END;
    RETURN z;
  END Add;


BEGIN
  One := NEW(T).fromArray(ARRAY OF R.T{R.One});
END Signal.
