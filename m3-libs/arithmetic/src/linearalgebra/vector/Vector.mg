GENERIC MODULE Vector(R, VR);
(* Arithmetic for Modula-3, see doc for details *)
FROM Arithmetic IMPORT Error;

<* UNUSED *>
CONST
  Module = "Vector.";


PROCEDURE New (n: CARDINAL; ): T =
  BEGIN
    RETURN NEW(T, n);
  END New;

PROCEDURE NewZero (n: CARDINAL; ): T =
  VAR z := NEW(T, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO z[i] := R.Zero; END;
    RETURN z;
  END NewZero;

PROCEDURE NewUniform (n: CARDINAL; x: R.T; ): T =
  VAR z := NEW(T, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO z[i] := x; END;
    RETURN z;
  END NewUniform;


PROCEDURE FromArray (READONLY x: TBody; ): T =
  VAR
    n := NUMBER(x);
    z := NEW(T, n);
  BEGIN
    z^ := x;
    RETURN z;
  END FromArray;


PROCEDURE FromVectorArray (READONLY x: TVBody; ): T =
  VAR size: CARDINAL := 0;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO INC(size, NUMBER(x[i]^)); END;
    VAR
      z            := NEW(T, size);
      iz: CARDINAL := 0;
    BEGIN
      FOR i := FIRST(x) TO LAST(x) DO
        SUBARRAY(z^, iz, NUMBER(x[i]^)) := x[i]^;
        INC(iz, NUMBER(x[i]^));
      END;
      RETURN z;
    END;
  END FromVectorArray;


PROCEDURE FromScalar (x: R.T; ): T =
  VAR z := NEW(T, 1);
  BEGIN
    z[0] := x;
    RETURN z;
  END FromScalar;


PROCEDURE Copy (x: T; ): T =
  VAR z := NEW(T, NUMBER(x^));
  BEGIN
    z^ := x^;
    RETURN z;
  END Copy;


PROCEDURE Reverse (x: T; ): T =
  VAR
    z           := NEW(T, NUMBER(x^));
    j: CARDINAL := LAST(x^) + 1;
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO DEC(j); z[i] := x[j]; END;
    RETURN z;
  END Reverse;



PROCEDURE IsZero (x: T; ): BOOLEAN =
  BEGIN
    RETURN VR.IsZero(x^);
  END IsZero;


PROCEDURE Equal (x, y: T; ): BOOLEAN =
  BEGIN
    RETURN VR.Equal(x^, y^);
  END Equal;


PROCEDURE Add (x, y: T; ): T =
  BEGIN
    RETURN VR.Add(x^, y^);
  END Add;


PROCEDURE Sub (x, y: T; ): T =
  BEGIN
    RETURN VR.Sub(x^, y^);
  END Sub;


PROCEDURE Neg (x: T; ): T =
  BEGIN
    RETURN VR.Neg(x^);
  END Neg;



PROCEDURE Scale (x: T; y: R.T; ): T =
  BEGIN
    RETURN VR.Scale(x^, y);
  END Scale;



PROCEDURE Inner (x, y: T; ): R.T =
  BEGIN
    RETURN VR.Inner(x^, y^);
  END Inner;


PROCEDURE Dot (x, y: T; ): R.T =
  BEGIN
    RETURN VR.Dot(x^, y^);
  END Dot;


PROCEDURE Apply (x: T; f: ApplyFtn; ) RAISES {Error} =
  BEGIN
    VR.Apply(x^, f);
  END Apply;

PROCEDURE Map (x: T; f: MapFtn; ): T RAISES {Error} =
  BEGIN
    RETURN VR.Map(x^, f);
  END Map;

PROCEDURE Reduce (x: T; f: ReduceFtn; init: R.T; ): R.T RAISES {Error} =
  BEGIN
    RETURN VR.Reduce(x^, f, init);
  END Reduce;


PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T; ): T =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := R.Add(from, by); END;
    END;
    RETURN x;
  END ArithSeq;


PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T; ): T =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := R.Mul(from, by); END;
    END;
    RETURN x;
  END GeomSeq;


PROCEDURE RecursiveSeq (num: CARDINAL; from: R.T; by: MapFtn; ): T
  RAISES {Error} =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := by(from); END;
    END;
    RETURN x;
  END RecursiveSeq;


BEGIN
END Vector.
