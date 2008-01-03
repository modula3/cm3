GENERIC MODULE VectorSupport(R);
(* Arithmetic for Modula-3, see doc for details *)

<* UNUSED *>
CONST
  Module = "VectorSupport.";


<* INLINE *>
PROCEDURE AssertEqualSize (READONLY x, y: T; ) =
  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(y), "Sizes of vectors must match." *>
  END AssertEqualSize;

PROCEDURE Clear (VAR z: T; ) =
  BEGIN
    FOR i := 0 TO LAST(z) DO z[i] := R.Zero; END;
  END Clear;

PROCEDURE Add (VAR z: T; READONLY x, y: T; ) =
  BEGIN
    AssertEqualSize(z, x);
    AssertEqualSize(z, y);
    FOR i := FIRST(z) TO LAST(z) DO z[i] := R.Add(x[i], y[i]); END;
  END Add;

PROCEDURE Sub (VAR z: T; READONLY x, y: T; ) =
  BEGIN
    AssertEqualSize(z, x);
    AssertEqualSize(z, y);
    FOR i := FIRST(z) TO LAST(z) DO z[i] := R.Sub(x[i], y[i]); END;
  END Sub;

PROCEDURE Neg (VAR z: T; READONLY x: T; ) =
  BEGIN
    AssertEqualSize(z, x);
    FOR i := FIRST(z) TO LAST(z) DO z[i] := R.Neg(x[i]); END;
  END Neg;



PROCEDURE Scale (VAR z: T; READONLY x: T; y: R.T; ) =
  BEGIN
    AssertEqualSize(z, x);
    FOR i := FIRST(z) TO LAST(z) DO z[i] := R.Mul(x[i], y); END;
  END Scale;



PROCEDURE Inner (READONLY x, y: T; ): R.T =
  VAR sum: R.T;
  BEGIN
    AssertEqualSize(x, y);
    sum := R.Zero;
    FOR i := FIRST(x) TO LAST(x) DO
      sum := R.Add(sum, R.Mul(R.Conj(x[i]), y[i]));
    END;
    RETURN sum;
  END Inner;



PROCEDURE Sum (READONLY x: T; ): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO sum := R.Add(sum, x[i]); END;
    RETURN sum;
  END Sum;


BEGIN
END VectorSupport.
