MODULE Signal;

REVEAL
  T = Public BRANDED OBJECT
	data : REF ARRAY OF ElemType;
	first : IndexType;
	OVERRIDES
	init      := Init;
	getfirst  := GetFirst;
	getlast   := GetLast;
	getnumber := GetNumber;

	translate := Translate;

    convolve   := Convolve;
	upsample   := UpSample;
	downsample := DownSample;
  END;


PROCEDURE Init (s : T; first, last : IndexType) =
  BEGIN
	s.data := NEW(REF ARRAY OF ElemType, last-first+1);
  END Init;

PROCEDURE GetFirst (s : T) : IndexType =
  BEGIN
	RETURN s.first;
  END GetFirst;

PROCEDURE GetLast (s : T) : IndexType =
  BEGIN
	RETURN s.first + NUMBER(s.data^)-1;
  END GetLast;

PROCEDURE GetNumber (s : T) : IndexType =
  BEGIN
	RETURN NUMBER(s.data);
  END GetNumber;


PROCEDURE Translate (s : T; dist : IndexType);
  BEGIN
	INC (s.first, dist);
  END Translate;



PROCEDURE Convolve (x : T; READONLY y : T) : REF T =
  VAR
	z : REF T;

  BEGIN
	z := NEW(REF T);
	z.init(x.getfirst()+y.getfirst(),x.getlast()+y.getlast());
	FOR i:=0 TO LAST(x.data) DO
	  FOR j:=0 TO LAST(y.data) DO
		z.data[i+j] := z.data[i+j] + x.data[i] + y.data[j];
	  END;
	END;
    RETURN z;
  END Convolve;


PROCEDURE UpSample (x : T; factor : IndexType) : REF T =
  VAR
	z : REF T;

  BEGIN
	z := NEW(REF T);
	z.init(x.getfirst()*factor,x.getlast()*factor);
	FOR i:=0 TO LAST(x.data) DO
      z.data[i*factor] := x.data[i];
	END;
  END UpSample;

PROCEDURE DownSample (x : T; factor : IndexType) : REF T =
  VAR
	z : REF T;

  BEGIN
	z := NEW(REF T);
	z.init (-((-x.getfirst()) DIV factor), x.getlast() DIV factor);
	FOR i:=z.first TO z.first+LAST(z.data) DO
      z.data[i-z.first] := x.data[i*factor-x.first];
	END;
  END DownSample;


BEGIN
END Signal.
