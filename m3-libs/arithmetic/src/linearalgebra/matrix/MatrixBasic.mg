GENERIC MODULE MatrixBasic(R, V, VR);
(*Arithmetic for Modula-3, see doc for details*)

IMPORT Arithmetic AS Arith;

CONST Module = "MatrixBasic.";

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
  VAR
    mf := 0;
    ml := LAST(x^);
    nf := 0;
    nl := LAST(x[0]);
  BEGIN
    FOR i := mf TO ml DO
      FOR j := nf TO nl DO
        IF NOT R.IsZero(x[i, j]) THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END IsZero;

(*----------------*)
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Arith.Error} =
  (*return x=y*)
  (*each is mxn*)
  <*UNUSED*>
  CONST ftn = Module & "Equal";
  VAR
    mf := 0;
    ml := LAST(x^);
    nf := 0;
    nl := LAST(x[0]);
  BEGIN
    AssertEqualSize(x, y);

    FOR i := mf TO ml DO
      FOR j := nf TO nl DO
        IF NOT R.Equal(x[i, j], y[i, j]) THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END Equal;

(*----------------*)
PROCEDURE Add (x, y: T): T RAISES {Arith.Error} =
  (*return x+y*)
  (*each is mxn*)
  <*UNUSED*>
  CONST ftn = Module & "Add";
  VAR
    m     := NUMBER(x^);
    mf    := 0;
    ml    := LAST(x^);
    n     := NUMBER(x[0]);
    nf    := 0;
    nl    := LAST(x[0]);
    z : T;
  BEGIN
    AssertEqualSize(x, y);

    z := NEW(T, m, n);
    FOR i := mf TO ml DO
      FOR j := nf TO nl DO z[i, j] := R.Add(x[i, j], y[i, j]); END;
    END;
    RETURN z;
  END Add;
(*----------------*)
PROCEDURE Sub (x, y: T): T RAISES {Arith.Error} =
  (*return x-y*)
  (*each is mxn*)
  <*UNUSED*>
  CONST ftn = Module & "Sub";
  VAR
    m     := NUMBER(x^);
    mf    := 0;
    ml    := LAST(x^);
    n     := NUMBER(x[0]);
    nf    := 0;
    nl    := LAST(x[0]);
    z : T;
  BEGIN
    AssertEqualSize(x, y);

    z := NEW(T, m, n);
    FOR i := mf TO ml DO
      FOR j := nf TO nl DO z[i, j] := R.Sub(x[i, j], y[i, j]); END;
    END;
    RETURN z;
  END Sub;

(*-----------------*)
PROCEDURE Scale (x: T; y: R.T): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x[0]));
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := FIRST(x[0]) TO LAST(x[0]) DO
        z[i, j] := R.Mul(x[i, j], y);
      END;
    END;
    RETURN z;
  END Scale;

(*-----------------*)
PROCEDURE Mul (x, y: T): T RAISES {Arith.Error} =
  (*return x*y*)
  (* x:mxn y:nxp return:mxp*)
  <*UNUSED*>
  CONST ftn = "Mul";
  VAR
    m     := NUMBER(x^);
    mf    := 0;
    ml    := m - 1;
    n     := NUMBER(x[0]);
    nf    := 0;
    nl    := n - 1;
    p     := NUMBER(y[0]);
    pf    := 0;
    pl    := p - 1;
    z : T;

  BEGIN
    IF NUMBER(y^) # n THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;
    z := NEW(T, m, p);
    FOR i := mf TO ml DO
      FOR j := pf TO pl DO
        VAR sum := R.Zero;
        BEGIN
          FOR k := nf TO nl DO
            sum := R.Add(sum, R.Mul(x[i, k], y[k, j]));
          END;
          z[i, j] := sum;
        END;
      END;
    END;
    RETURN z;
  END Mul;

(*-----------------*)
PROCEDURE MulV (A: T; b: V.T): V.T RAISES {Arith.Error} =

  <*UNUSED*>
  CONST ftn = Module & "MulV";
  VAR
    m  := NUMBER(A^);
    mf := 0;
    ml := m - 1;
    c  := NEW(V.T, m);
  BEGIN
    IF NUMBER(A[0]) # NUMBER(b^) THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;

    FOR i := mf TO ml DO c[i] := VR.Dot(A[i], b^); END;
    RETURN c;
  END MulV;

(*-----------------*)
PROCEDURE MulTV (A: T; b: V.T): V.T RAISES {Arith.Error} =

  <*UNUSED*>
  CONST ftn = Module & "MulTV";
  VAR
    mf := 0;
    ml := LAST(A^);
    nf := 0;
    nl := LAST(A[0]);
    c  := NEW(V.T, NUMBER(A[0]));
  BEGIN
    IF NUMBER(A^) # NUMBER(b^) THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;

    FOR i := nf TO nl DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := mf TO ml DO sum := R.Add(sum, R.Mul(A[j, i], b[j])); END;
        c[i] := sum;
      END;
    END;
    RETURN c;
  END MulTV;

(*-----------------*)
PROCEDURE Transpose (x: T): T =
  <*UNUSED*>
  CONST ftn = Module & "Transpose";
  VAR
    m     := NUMBER(x^);
    mf    := 0;
    ml    := m - 1;
    n     := NUMBER(x[0]);
    nf    := 0;
    nl    := n - 1;
    z : T;
  BEGIN
    z := NEW(T, n, m);
    FOR i := nf TO nl DO FOR j := mf TO ml DO z[i, j] := x[j, i]; END; END;
    RETURN z;
  END Transpose;

(*-----------------*)
PROCEDURE Adjoint (x: T): T =
  <*UNUSED*>
  CONST ftn = Module & "Adjoint";
  VAR
    m     := NUMBER(x^);
    mf    := 0;
    ml    := m - 1;
    n     := NUMBER(x[0]);
    nf    := 0;
    nl    := n - 1;
    z : T;
  BEGIN
    z := NEW(T, n, m);
    FOR i := nf TO nl DO
      FOR j := mf TO ml DO z[i, j] := R.Conj(x[j, i]); END;
    END;
    RETURN z;
  END Adjoint;

(*-----------------*)
PROCEDURE MulMAM (x: T): T =
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

(*-----------------*)
PROCEDURE MulMMA (x: T): T =
  VAR z := NEW(T, NUMBER(x^), NUMBER(x^));
  BEGIN
    FOR i := 0 TO LAST(x^) DO
      FOR j := i TO LAST(x^) DO
        <*FATAL Arith.Error*>(*x[i] and x[j] will have the same size*)
        BEGIN
          z[i, j] := VR.Dot(x[i], x[j]);
        END;
        z[j, i] := R.Conj(z[i, j]);
      END;
    END;
    RETURN z;
  END MulMMA;

(*-----------------*)
PROCEDURE Trace (x: T): R.T =
  VAR y: R.T;
  BEGIN
    y := x[0, 0];
    FOR j := 1 TO MIN(LAST(x^), LAST(x[0])) DO y := R.Add(y, x[j, j]); END;
    RETURN y;
  END Trace;


(*-----------------*)
(*PROCEDURE Determinant(x:T):R.T;*)

(*-----------------*)
BEGIN
END MatrixBasic.
