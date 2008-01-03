GENERIC MODULE MatrixBasic(R, V, VR);
(* Arithmetic for Modula-3, see doc for details *)

<* INLINE *>
PROCEDURE AssertEqualSize (x, y: T; ) =
  BEGIN
    <* ASSERT NUMBER(x^) = NUMBER(y^) AND NUMBER(x[0]) = NUMBER(y[0]),
                "Sizes of matrices must match." *>
  END AssertEqualSize;

<* INLINE *>
PROCEDURE AssertEqualWidth (n, m: CARDINAL; ) =
  BEGIN
    <* ASSERT n = m, "Width or height of operands don't match." *>
  END AssertEqualWidth;

PROCEDURE IsZero (x: T; ): BOOLEAN =
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF NOT R.IsZero(x[i, j]) THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END IsZero;

PROCEDURE Equal (x, y: T; ): BOOLEAN =
  BEGIN
    AssertEqualSize(x, y);

    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF NOT R.Equal(x[i, j], y[i, j]) THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END Equal;

PROCEDURE Add (x, y: T; ): T =
  BEGIN
    AssertEqualSize(x, y);

    WITH z = NEW(T, NUMBER(x^), NUMBER(x[0])) DO
      FOR i := FIRST(x^) TO LAST(x^) DO
        FOR j := FIRST(x[0]) TO LAST(x[0]) DO
          z[i, j] := R.Add(x[i, j], y[i, j]);
        END;
      END;
      RETURN z;
    END;
  END Add;

PROCEDURE Sub (x, y: T; ): T =
  BEGIN
    AssertEqualSize(x, y);

    WITH z = NEW(T, NUMBER(x^), NUMBER(x[0])) DO
      FOR i := FIRST(x^) TO LAST(x^) DO
        FOR j := FIRST(x[0]) TO LAST(x[0]) DO
          z[i, j] := R.Sub(x[i, j], y[i, j]);
        END;
      END;
      RETURN z;
    END;
  END Sub;

PROCEDURE Scale (x: T; y: R.T; ): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x[0]));
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        z[i, j] := R.Mul(x[i, j], y);
      END;
    END;
    RETURN z;
  END Scale;

PROCEDURE Mul (x, y: T; ): T =
  VAR
    m    := NUMBER(x^);
    n    := NUMBER(x[0]);
    p    := NUMBER(y[0]);
    z: T;

  BEGIN
    AssertEqualWidth(NUMBER(y^), n);
    z := NEW(T, m, p);
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(y[0]) TO LAST(y[0]) DO
        VAR sum := R.Zero;
        BEGIN
          FOR k := FIRST(x[0]) TO LAST(x[0]) DO
            sum := R.Add(sum, R.Mul(x[i, k], y[k, j]));
          END;
          z[i, j] := sum;
        END;
      END;
    END;
    RETURN z;
  END Mul;

PROCEDURE MulV (A: T; b: V.T; ): V.T =
  BEGIN
    AssertEqualWidth(NUMBER(A[0]), NUMBER(b^));

    WITH c = NEW(V.T, NUMBER(A^)) DO
      FOR i := FIRST(A^) TO LAST(A^) DO c[i] := VR.Dot(A[i], b^); END;
      RETURN c;
    END;
  END MulV;

PROCEDURE MulTV (A: T; b: V.T; ): V.T =
  BEGIN
    AssertEqualWidth(NUMBER(A^), NUMBER(b^));

    WITH c = NEW(V.T, NUMBER(A[0])) DO
      FOR i := FIRST(A[0]) TO LAST(A[0]) DO
        VAR sum := R.Zero;
        BEGIN
          FOR j := FIRST(A^) TO LAST(A^) DO
            sum := R.Add(sum, R.Mul(A[j, i], b[j]));
          END;
          c[i] := sum;
        END;
      END;
      RETURN c;
    END;
  END MulTV;

PROCEDURE Adjoint (x: T; ): T =
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x^));
  BEGIN
    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      FOR j := FIRST(x^) TO LAST(x^) DO z[i, j] := R.Conj(x[j, i]); END;
    END;
    RETURN z;
  END Adjoint;

PROCEDURE MulMAM (x: T; ): T =
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x[0]));
  BEGIN
    FOR i := 0 TO LAST(x[0]) DO
      FOR j := i TO LAST(x[0]) DO
        VAR sum := R.Mul(R.Conj(x[0, i]), x[0, j]);
        BEGIN
          FOR k := 1 TO LAST(x^) DO
            sum := R.Add(sum, R.Mul(R.Conj(x[k, i]), x[k, j]));
          END;
          z[i, j] := sum;
          z[j, i] := R.Conj(sum);
        END;
      END;
    END;
    RETURN z;
  END MulMAM;

PROCEDURE MulMMA (x: T; ): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x^));
  BEGIN
    FOR i := 0 TO LAST(x^) DO
      FOR j := i TO LAST(x^) DO
        z[i, j] := VR.Dot(x[i], x[j]);
        z[j, i] := R.Conj(z[i, j]);
      END;
    END;
    RETURN z;
  END MulMMA;

PROCEDURE Trace (x: T; ): R.T =
  VAR y: R.T;
  BEGIN
    y := x[0, 0];
    FOR j := 1 TO MIN(LAST(x^), LAST(x[0])) DO y := R.Add(y, x[j, j]); END;
    RETURN y;
  END Trace;


BEGIN
END MatrixBasic.
