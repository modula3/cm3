GENERIC MODULE MatrixFast(R, V, VR);
(**
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic matrices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL32.

1/27/96   Harry George
          Converted to OO format and R.T

2/17/96   Harry George   ...and back to ADT format
**)

FROM NADefinitions IMPORT Error, Err;

CONST Module = "MatrixFast.";

(*
(*-----------------*)
PROCEDURE Zero(
                x:T)=
(*set all zeros*)
VAR
  m:=NUMBER(x^);    mf:=0; ml:=m-1;
  n:=NUMBER(x[0]); nf:=0; nl:=n-1;
BEGIN
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      x[i,j]:=R.Zero;
    END;
  END;
END Zero;
(*-----------------*)
PROCEDURE One(
               x:T) RAISES {Error} =
(*set all zeros except diagonal to 1's*)
<*UNUSED*> <*UNUSED*> CONST ftn = "Midentity";
VAR
  m:=NUMBER(x^);    mf:=0; ml:=m-1;
  n:=NUMBER(x[0]); nf:=0; nl:=n-1;
BEGIN
  IF m # n THEN
    RAISE Error(Err.bad_size);
  END;
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      x[i,j]:=R.Zero;
    END;
  END;
  FOR i:=mf TO ml DO
    x[i,i]:=R.One;
  END;
END One;
*)


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize (x, y: T) RAISES {Error} =
  BEGIN
    IF NUMBER(x^) # NUMBER(y^) OR NUMBER(x[0]) # NUMBER(y[0]) THEN
      RAISE Error(Err.bad_size);
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
      FOR j := nf TO nl DO IF x[i, j] # R.Zero THEN RETURN FALSE; END; END;
    END;
    RETURN TRUE;
  END IsZero;
(*----------------*)
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Error} =
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
        IF x[i, j] # y[i, j] THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END Equal;

(*----------------*)
PROCEDURE Add (x, y: T): T RAISES {Error} =
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
      FOR j := nf TO nl DO z[i, j] := x[i, j] + y[i, j]; END;
    END;
    RETURN z;
  END Add;
(*----------------*)
PROCEDURE Sub (x, y: T): T RAISES {Error} =
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
      FOR j := nf TO nl DO z[i, j] := x[i, j] - y[i, j]; END;
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
PROCEDURE Mul (x, y: T): T RAISES {Error} =
  <*UNUSED*>
  CONST ftn = "Mul";
  VAR
    m        := NUMBER(x^);
    mf       := 0;
    ml       := m - 1;
    n        := NUMBER(x[0]);
    nf       := 0;
    nl       := n - 1;
    p        := NUMBER(y[0]);
    pf       := 0;
    pl       := p - 1;
    z  : T;
    sum: R.T;

  BEGIN
    IF NUMBER(y^) # n THEN RAISE Error(Err.bad_size); END;
    z := NEW(T, m, p);
    FOR i := mf TO ml DO
      FOR j := pf TO pl DO
        sum := R.Zero;
        FOR k := nf TO nl DO sum := sum + x[i, k] * y[k, j]; END;
        z[i, j] := sum;
      END;
    END;
    RETURN z;
  END Mul;

(*----------------*)
PROCEDURE MulV (A: T; b: V.T): V.T RAISES {Error} =
  <*UNUSED*>
  CONST ftn = Module & "MulV";
  VAR
    m        := NUMBER(A^);
    mf       := 0;
    ml       := m - 1;
    n        := NUMBER(A[0]);
    nf       := 0;
    nl       := n - 1;
    c        := NEW(V.T, m);
    sum: R.T;
  BEGIN
    IF NUMBER(b^) # n THEN RAISE Error(Err.bad_size); END;

    FOR i := mf TO ml DO
      sum := R.Zero;
      FOR j := nf TO nl DO sum := sum + b[j] * A[i, j]; END;
      c[i] := sum;
    END;
    RETURN c;
  END MulV;

(*-----------------*)
PROCEDURE MulTV (A: T; b: V.T): V.T RAISES {Error} =
  <*UNUSED*>
  CONST ftn = Module & "MulTV";
  VAR
    mf := 0;
    ml := LAST(A^);
    nf := 0;
    nl := LAST(A[0]);
    c  := NEW(V.T, NUMBER(A[0]));
  BEGIN
    IF NUMBER(b^) # NUMBER(A^) THEN RAISE Error(Err.bad_size); END;

    FOR i := nf TO nl DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := mf TO ml DO sum := sum + A[j, i] * b[j]; END;
        c[i] := sum;
      END;
    END;
    RETURN c;
  END MulTV;

(*-----------------*)
PROCEDURE Transpose (x: T): T =
  <*UNUSED*>
  CONST ftn = Module & "mTranspose";
  VAR
    m  := NUMBER(x^);
    mf := 0;
    ml := m - 1;
    n  := NUMBER(x[0]);
    nf := 0;
    nl := n - 1;
    z  := NEW(T, n, m);
  BEGIN
    FOR i := nf TO nl DO FOR j := mf TO ml DO z[i, j] := x[j, i]; END; END;
    RETURN z;
  END Transpose;

(*-----------------*)
PROCEDURE MulMAM (x: T): T =
  VAR z := New(NUMBER(x[0]), NUMBER(x[0]));
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
  VAR z := New(NUMBER(x^), NUMBER(x^));
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
  VAR y: R.T := R.Zero;
  BEGIN
    FOR j := 0 TO MIN(LAST(x^), LAST(x[0])) DO y := y + x[j, j]; END;
    RETURN y;
  END Trace;

(*-----------------*)
BEGIN
END MatrixFast.
