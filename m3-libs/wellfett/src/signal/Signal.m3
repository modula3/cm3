MODULE Signal;

REVEAL
  T = Public BRANDED OBJECT
	data : REF ARRAY OF ElemType;
	first : IndexType;
	OVERRIDES
	init      := Init;
	copy      := Copy;

	getfirst  := GetFirst;
	getlast   := GetLast;
	getnumber := GetNumber;

	translate  := Translate;
	upsample   := UpSample;
	downsample := DownSample;
	wrapcyclic := WrapCyclic;

    scale      := Scale;
    raise      := Raise;

    convolve   := Convolve;
    superpose  := Superpose;
  END;


PROCEDURE Init (SELF : T; first, last : IndexType) =
  BEGIN
	SELF.data := NEW(REF ARRAY OF ElemType, last-first);
  END Init;

PROCEDURE Copy (SELF : T) : T =
  VAR
	c : T;
  BEGIN
	c := NEW(T);
	c.init(SELF.first,SELF.first+NUMBER(SELF.data^));
	FOR j:=FIRST(SELF.data^) TO LAST(SELF.data^) DO
	  c.data[j] := SELF.data[j];
	END;
	RETURN c;
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


PROCEDURE Translate (SELF : T; dist : IndexType) =
  BEGIN
	INC (SELF.first, dist);
  END Translate;

PROCEDURE UpSample (x : T; factor : IndexType) : T =
  VAR
	z : T;

  BEGIN
	z := NEW(T);
	z.init(x.getfirst()*factor,(x.getlast()-1)*factor+1);
	FOR i:=0 TO LAST(x.data^) DO
      z.data[i*factor] := x.data[i];
	END;
	RETURN z;
  END UpSample;

PROCEDURE DownSample (x : T; factor : IndexType) : T =
  VAR
	z : T;

  BEGIN
	z := NEW(T);
	z.init (-((-x.getfirst()) DIV factor), (x.getlast()-1) DIV factor +1);
	FOR i:=z.first TO z.first+LAST(z.data^) DO
      z.data[i-z.first] := x.data[i*factor-x.first];
	END;
	RETURN z;
  END DownSample;

PROCEDURE WrapCyclic (x : T; length : IndexType) : T =
  VAR
	z : T;
	j : IndexType;

  BEGIN
	z := NEW(T);
	z.init (0, length);
	j := x.first MOD length;
	FOR i:=0 TO LAST(z.data^) DO
      z.data[j] := z.data[j] + x.data[i];
	  INC(j);
	  IF j>=length THEN
		j:=0;
      END;
	END;
	RETURN z;
  END WrapCyclic;


PROCEDURE Scale (x : T; factor : ElemType) =
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
      x.data[i] := x.data[i] * factor;
	END;
  END Scale;

PROCEDURE Raise (x : T; offset : ElemType) =
  BEGIN
	FOR i:=0 TO LAST(x.data^) DO
      x.data[i] := x.data[i] + offset;
	END;
  END Raise;


PROCEDURE Convolve (x : T; y : T) : T =
  VAR
	z : T;

  BEGIN
	z := NEW(T);
	z.init(x.getfirst()+y.getfirst(),x.getlast()+y.getlast());
	FOR i:=0 TO LAST(x.data^) DO
	  FOR j:=0 TO LAST(y.data^) DO
		z.data[i+j] := z.data[i+j] + x.data[i] + y.data[j];
	  END;
	END;
    RETURN z;
  END Convolve;

PROCEDURE Superpose (x : T; y : T) : T =
  VAR
	z : T;
	j : IndexType;

  BEGIN
	z := NEW(T);
	z.init(MIN(x.getfirst(),y.getfirst()),MAX(x.getlast(),y.getlast()));
	j := x.getfirst()-z.getfirst();
	FOR i:=0 TO LAST(x.data^) DO
	  z.data[i+j] := z.data[i+j] + x.data[i];
	END;
	j := y.getfirst()-z.getfirst();
	FOR i:=0 TO LAST(y.data^) DO
	  z.data[i+j] := z.data[i+j] + y.data[i];
	END;
    RETURN z;
  END Superpose;


BEGIN
END Signal.
