GENERIC MODULE Matrix(R, V);
(* Arithmetic for Modula-3, see doc for details *)


<* UNUSED *>
CONST
  Module = "Matrix.";

PROCEDURE New (m, n: CARDINAL; ): T =
  BEGIN
    RETURN NEW(T, m, n);
  END New;

PROCEDURE FromArray (READONLY x: TBody; ): T =
  VAR
    m := NUMBER(x);
    n := NUMBER(x[0]);
    z := NEW(T, m, n);
  BEGIN
    z^ := x;
    RETURN z;
  END FromArray;

PROCEDURE FromMatrixArray (READONLY x: TMBody; ): T =
  BEGIN
    IF NUMBER(x) = 0 OR NUMBER(x[0]) = 0 THEN
      RETURN New(0, 0);
    ELSE
      VAR m, n: CARDINAL := 0;
      BEGIN
        (* check matching row numbers and sum them up *)
        FOR i := 0 TO LAST(x) DO
          WITH size = NUMBER(x[i, 0]^) DO
            FOR j := 1 TO LAST(x[0]) DO
              <* ASSERT size = NUMBER(x[i, j]^), "Row numbers don't match." *>
            END;
            INC(m, size);
          END;
        END;
        (* check matching column numbers and sum them up *)
        FOR j := 0 TO LAST(x[0]) DO
          WITH size = NUMBER(x[0, j][0]) DO
            FOR i := 1 TO LAST(x) DO
              <* ASSERT size = NUMBER(x[i, j][0]),
                          "Column numbers don't match." *>
            END;
            INC(n, size);
          END;
        END;

        VAR
          z                := New(m, n);
          iz, jz: CARDINAL := 0;
        BEGIN
          FOR i := 0 TO LAST(x) DO
            jz := 0;
            FOR j := 0 TO LAST(x[0]) DO
              WITH y = x[i, j] DO
                FOR k := 0 TO LAST(y^) DO
                  SUBARRAY(z[iz + k], jz, NUMBER(y[0])) := y[k];
                END;
                INC(jz, NUMBER(y[0]));
              END;
            END;
            INC(iz, NUMBER(x[i, 0]^));
          END;
          RETURN z;
        END;
      END;
    END;
  END FromMatrixArray;

PROCEDURE RowFromArray (READONLY x: V.TBody; ): T =
  VAR z := NEW(T, 1, NUMBER(x));
  BEGIN
    z[0] := x;
    RETURN z;
  END RowFromArray;

PROCEDURE ColumnFromArray (READONLY x: V.TBody; ): T =
  VAR z := NEW(T, NUMBER(x), 1);
  BEGIN
    FOR i := 0 TO LAST(x) DO z[i, 0] := x[i]; END;
    RETURN z;
  END ColumnFromArray;

PROCEDURE DiagonalFromArray (READONLY x: V.TBody; ): T =
  VAR z := NEW(T, NUMBER(x), NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      z[i, i] := x[i];
      FOR j := FIRST(x) TO i - 1 DO
        z[i, j] := R.Zero;
        z[j, i] := R.Zero;
      END;
    END;
    RETURN z;
  END DiagonalFromArray;

PROCEDURE RowFromVector (x: V.T; ): T =
  BEGIN
    RETURN RowFromArray(x^);
  END RowFromVector;

PROCEDURE ColumnFromVector (x: V.T; ): T =
  BEGIN
    RETURN ColumnFromArray(x^);
  END ColumnFromVector;

PROCEDURE DiagonalFromVector (x: V.T; ): T =
  BEGIN
    RETURN DiagonalFromArray(x^);
  END DiagonalFromVector;

PROCEDURE FromScalar (x: R.T; ): T =
  VAR z := NEW(T, 1, 1);
  BEGIN
    z[0, 0] := x;
    RETURN z;
  END FromScalar;

PROCEDURE Copy (x: T; ): T =
  VAR
    m := NUMBER(x^);
    n := NUMBER(x[0]);
    z := NEW(T, m, n);
  BEGIN
    z^ := x^;
    RETURN z;
  END Copy;


PROCEDURE NewZero (m, n: CARDINAL; ): T =
  VAR
    mf := 0;
    ml := m - 1;
    nf := 0;
    nl := n - 1;
    z  := NEW(T, m, n);
  BEGIN
    FOR i := mf TO ml DO FOR j := nf TO nl DO z[i, j] := R.Zero; END; END;
    RETURN z;
  END NewZero;

PROCEDURE NewOne (n: CARDINAL; ): T =
  VAR
    nf := 0;
    nl := n - 1;
    z  := NEW(T, n, n);
  BEGIN
    FOR i := nf TO nl DO
      z[i, i] := R.One;
      FOR j := nf TO i - 1 DO z[i, j] := R.Zero; z[j, i] := R.Zero; END;
    END;
    RETURN z;
  END NewOne;

PROCEDURE Cyclic (x: V.T; size: CARDINAL; shift: INTEGER; ): T =
  VAR
    z             := New(size, NUMBER(x^));
    rem: CARDINAL;
  BEGIN
    shift := shift MOD NUMBER(x^);
    rem := NUMBER(x^) - shift;
    IF size > 0 THEN
      z[0] := x^;
      FOR i := 1 TO LAST(z^) DO
        SUBARRAY(z[i], 0, shift) := SUBARRAY(z[i - 1], rem, shift);
        SUBARRAY(z[i], shift, rem) := SUBARRAY(z[i - 1], 0, rem);
      END;
    END;
    RETURN z;
  END Cyclic;

PROCEDURE Transpose (x: T; ): T =
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x^));
  BEGIN
    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      FOR j := FIRST(x^) TO LAST(x^) DO z[i, j] := x[j, i]; END;
    END;
    RETURN z;
  END Transpose;



PROCEDURE GetRow (x: T; k: CARDINAL; ): V.T =
  VAR y := V.New(NUMBER(x[0]));
  BEGIN
    y^ := x[k];
    RETURN y;
  END GetRow;

PROCEDURE GetColumn (x: T; k: CARDINAL; ): V.T =
  VAR y := V.New(NUMBER(x^));
  BEGIN
    FOR j := 0 TO LAST(y^) DO y[j] := x[j, k]; END;
    RETURN y;
  END GetColumn;


PROCEDURE Apply (x: T; f: ApplyFtn; ) =
  BEGIN
    FOR i := 0 TO LAST(x^) DO
      FOR j := 0 TO LAST(x[0]) DO f(x[i, j]); END;
    END;
  END Apply;

PROCEDURE Map (x: T; f: MapFtn; ): T =
  VAR y := NEW(T, NUMBER(x^), NUMBER(x[0]));
  BEGIN
    FOR i := 0 TO LAST(x^) DO
      FOR j := 0 TO LAST(x[0]) DO y[i, j] := f(x[i, j]); END;
    END;
    RETURN y;
  END Map;

PROCEDURE ReduceRows (x: T; f: ReduceFtn; READONLY init: V.TBody; ): V.T =
  VAR y := NEW(V.T, NUMBER(init));
  BEGIN
    y^ := init;
    FOR i := 0 TO LAST(x^) DO
      FOR j := 0 TO LAST(x[0]) DO x[i, j] := f(y[i], x[i, j]); END;
    END;
    RETURN y;
  END ReduceRows;

PROCEDURE ReduceColumns (x: T; f: ReduceFtn; READONLY init: V.TBody; ):
  V.T =
  VAR y := NEW(V.T, NUMBER(init));
  BEGIN
    y^ := init;
    FOR i := 0 TO LAST(x^) DO
      FOR j := 0 TO LAST(x[0]) DO x[i, j] := f(y[j], x[i, j]); END;
    END;
    RETURN y;
  END ReduceColumns;

BEGIN
END Matrix.
