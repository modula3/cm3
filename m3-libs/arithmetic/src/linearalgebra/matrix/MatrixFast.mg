GENERIC MODULE MatrixFast(R, V, VS);
(*Arithmetic for Modula-3, see doc for details*)

IMPORT Arithmetic AS Arith;

CONST Module = "MatrixFast.";

<*UNUSED*>
CONST Dummy = VS.Inner;

(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize (x, y: T) RAISES {Arith.Error} =
  BEGIN
    IF NUMBER(x^) # NUMBER(y^) OR NUMBER(x[0]) # NUMBER(y[0]) THEN
      RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init());
    END;
  END AssertEqualSize;

(*----------------*)
PROCEDURE IsZero (x: T): BOOLEAN =
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF x[i, j] # R.Zero THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END IsZero;
(*----------------*)
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = Module & "Equal";
  BEGIN
    AssertEqualSize(x, y);

    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        IF x[i, j] # y[i, j] THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END Equal;

(*----------------*)
PROCEDURE Add (x, y: T): T RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = Module & "Add";
  VAR
    m    := NUMBER(x^);
    n    := NUMBER(x[0]);
    z: T;
  BEGIN
    AssertEqualSize(x, y);

    z := NEW(T, m, n);
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        z[i, j] := x[i, j] + y[i, j];
      END;
    END;
    RETURN z;
  END Add;
(*----------------*)
PROCEDURE Sub (x, y: T): T RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = Module & "Sub";
  VAR
    m    := NUMBER(x^);
    n    := NUMBER(x[0]);
    z: T;
  BEGIN
    AssertEqualSize(x, y);

    z := NEW(T, m, n);
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        z[i, j] := x[i, j] - y[i, j];
      END;
    END;
    RETURN z;
  END Sub;

(*-----------------*)
PROCEDURE Scale (x: T; y: R.T): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x[0]));
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO z[i, j] := x[i, j] * y; END;
    END;
    RETURN z;
  END Scale;

(*-----------------*)
PROCEDURE Mul (x, y: T): T RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = "Mul";
  VAR
    m    := NUMBER(x^);
    n    := NUMBER(x[0]);
    p    := NUMBER(y[0]);
    z: T;

  BEGIN
    IF NUMBER(y^) # n THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;
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

(*----------------*)
PROCEDURE MulV (x: T; y: V.T): V.T RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = Module & "MulV";
  VAR z := NEW(V.T, NUMBER(x^));
  BEGIN
    IF NUMBER(x[0]) # NUMBER(y^) THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;

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

(*-----------------*)
PROCEDURE MulTV (x: T; y: V.T): V.T RAISES {Arith.Error} =
  <*UNUSED*>
  CONST ftn = Module & "MulTV";
  VAR z := NEW(V.T, NUMBER(x[0]));
  BEGIN
    IF NUMBER(x^) # NUMBER(y^) THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;

    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := FIRST(x^) TO LAST(x^) DO sum := sum + x[j, i] * y[j]; END;
        z[i] := sum;
      END;
    END;
    RETURN z;
  END MulTV;

(*-----------------*)
PROCEDURE Transpose (x: T): T =
  <*UNUSED*>
  CONST ftn = Module & "mTranspose";
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x^));
  BEGIN
    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      FOR j := FIRST(x^) TO LAST(x^) DO z[i, j] := x[j, i]; END;
    END;
    RETURN z;
  END Transpose;

(*-----------------*)
PROCEDURE MulMAM (x: T): T =
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

(*-----------------*)
PROCEDURE MulMMA (x: T): T =
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

(*-----------------*)
PROCEDURE Trace (x: T): R.T =
  VAR y := R.Zero;
  BEGIN
    FOR j := 0 TO MIN(LAST(x^), LAST(x[0])) DO y := y + x[j, j]; END;
    RETURN y;
  END Trace;

(*-----------------*)
BEGIN
END MatrixFast.
