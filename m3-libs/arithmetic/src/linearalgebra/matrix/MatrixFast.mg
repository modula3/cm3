GENERIC MODULE MatrixFast(R, V, MS, MD);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Arithmetic;


<* INLINE *>
PROCEDURE AssertEqualSize (x, y: T; ) =
  BEGIN
    <* ASSERT NUMBER(x^) = NUMBER(y^) AND NUMBER(x[0]) = NUMBER(y[0]),
                "Sizes of matrices must match." *>
  END AssertEqualSize;


PROCEDURE IsZero (x: T; ): BOOLEAN =
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF x[i, j] # R.Zero THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END IsZero;

PROCEDURE Equal (x, y: T; ): BOOLEAN =
  BEGIN
    AssertEqualSize(x, y);

    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF x[i, j] # y[i, j] THEN RETURN FALSE; END;
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
          z[i, j] := x[i, j] + y[i, j];
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
          z[i, j] := x[i, j] - y[i, j];
        END;
      END;
      RETURN z;
    END;
  END Sub;

PROCEDURE Scale (x: T; y: R.T; ): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x[0]));
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO z[i, j] := x[i, j] * y; END;
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
    MS.AssertEqualWidth(NUMBER(y^), n);
    z := NEW(T, m, p);
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(y[0]) TO LAST(y[0]) DO
        VAR sum := R.Zero;
        BEGIN
          FOR k := FIRST(x[0]) TO LAST(x[0]) DO
            sum := sum + x[i, k] * y[k, j];
          END;
          z[i, j] := sum;
        END;
      END;
    END;
    RETURN z;
  END Mul;

PROCEDURE MulV (x: T; y: V.T; ): V.T =
  VAR z := NEW(V.T, NUMBER(x^));
  BEGIN
    MS.AssertEqualWidth(NUMBER(x[0]), NUMBER(y^));

    FOR i := FIRST(x^) TO LAST(x^) DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := FIRST(x[0]) TO LAST(x[0]) DO
          sum := sum + y[j] * x[i, j];
        END;
        z[i] := sum;
      END;
    END;
    RETURN z;
  END MulV;

PROCEDURE MulTV (x: T; y: V.T; ): V.T =
  VAR z := NEW(V.T, NUMBER(x[0]));
  BEGIN
    MS.AssertEqualWidth(NUMBER(x^), NUMBER(y^));

    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := FIRST(x^) TO LAST(x^) DO sum := sum + x[j, i] * y[j]; END;
        z[i] := sum;
      END;
    END;
    RETURN z;
  END MulTV;

PROCEDURE MulMAM (x: T; ): T =
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x[0]));
  BEGIN
    FOR i := 0 TO LAST(x[0]) DO
      FOR j := i TO LAST(x[0]) DO
        VAR sum := R.Zero;
        BEGIN
          FOR k := 0 TO LAST(x^) DO sum := sum + x[k, i] * x[k, j]; END;
          z[i, j] := sum;
          z[j, i] := sum;
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
        VAR sum := R.Zero;
        BEGIN
          FOR k := 0 TO LAST(x[0]) DO sum := sum + x[i, k] * x[j, k]; END;
          z[i, j] := sum;
          z[j, i] := sum;
        END;
      END;
    END;
    RETURN z;
  END MulMMA;

PROCEDURE Trace (x: T; ): R.T =
  VAR y := R.Zero;
  BEGIN
    FOR j := 0 TO MIN(LAST(x^), LAST(x[0])) DO y := y + x[j, j]; END;
    RETURN y;
  END Trace;

(* CONST Adjoint = M.Transpose; *)


PROCEDURE Determinant (x: T; ): R.T =
  BEGIN
    TRY
      RETURN MD.LUDet(MD.LUFactor(x));
    EXCEPT
    | Arithmetic.Error =>
        (* Although the determinant is always defined, the fast Gauﬂ
           algorithm fails sometimes.  In this cases we fall back to the
           division-free algorithm. *)
        RETURN MS.Determinant(x);
    END;
  END Determinant;

BEGIN
END MatrixFast.
