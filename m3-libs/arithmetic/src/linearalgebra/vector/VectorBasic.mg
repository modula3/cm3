GENERIC MODULE VectorBasic(R, V);
(*Arithmetic for Modula-3, see doc for details *)
IMPORT Arithmetic AS Arith;

(*VR is needed to reveal the structure of V.T*)

<*UNUSED*>
CONST Module = "VectorBasic.";


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize (READONLY x, y: T) RAISES {Arith.Error} =
  BEGIN
    IF NUMBER(x) # NUMBER(y) THEN RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init()); END;
  END AssertEqualSize;

(*---------------------*)
PROCEDURE IsZero (READONLY x: T): BOOLEAN =
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      IF NOT R.IsZero(x[i]) THEN RETURN FALSE; END
    END;
    RETURN TRUE;
  END IsZero;

(*---------------------*)
PROCEDURE Equal (READONLY x, y: T): BOOLEAN RAISES {Arith.Error} =
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO
      IF NOT R.Equal(x[i], y[i]) THEN RETURN FALSE; END
    END;
    RETURN TRUE;
  END Equal;

(*-----------------*)
PROCEDURE Add (READONLY x, y: T): V.T RAISES {Arith.Error} =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO z[i] := R.Add(x[i], y[i]); END;
    RETURN z;
  END Add;

(*-----------------*)
PROCEDURE Sub (READONLY x, y: T): V.T RAISES {Arith.Error} =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO z[i] := R.Sub(x[i], y[i]); END;
    RETURN z;
  END Sub;

(*---------------------*)
PROCEDURE Neg (READONLY x: T): V.T =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := R.Neg(x[i]); END;
    RETURN z;
  END Neg;


(*-----------------*)
PROCEDURE Scale (READONLY x: T; y: R.T): V.T =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := R.Mul(x[i], y); END;
    RETURN z;
  END Scale;


(*-----------------*)
PROCEDURE Inner (READONLY x, y: T): R.T RAISES {Arith.Error} =
  VAR sum := R.Zero;
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO
      sum := R.Add(sum, R.Mul(R.Conj(x[i]), y[i]));
    END;
    RETURN sum;
  END Inner;

(*-----------------*)
PROCEDURE Dot (READONLY x, y: T): R.T RAISES {Arith.Error} =
  VAR sum := R.Zero;
  BEGIN
    AssertEqualSize(x, y);
    FOR i := FIRST(x) TO LAST(x) DO
      sum := R.Add(sum, R.Mul(x[i], y[i]));
    END;
    RETURN sum;
  END Dot;

(*-----------------*)
(**
   should be generalized to finding an orthonormal basis
   of the space orthogonal to a given set of vectors
   one way to do this:
     let the matrix have size (n,m) with less columns than rows (m<n)
     clip the matrix to size (m+1,m) and create a column vector orthogonal to it
     the j-th component is computed by the determinant of the limitted matrix
     with the j-th row removed
     now iterate to the matrix of size (m+2,m+1) and so on

   for floating point numbers this can be done more efficiently
   by a QR factorization

PROCEDURE Cross(
                x,y:T):T RAISES {Arith.Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
**)

PROCEDURE Sum (READONLY x: T): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO sum := R.Add(sum, x[i]); END;
    RETURN sum;
  END Sum;

(*-----------------*)
PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): V.T =
  VAR x := NEW(V.T, num);
  BEGIN
    IF num > 0 THEN
      x[0] := from;
      FOR j := 1 TO num - 1 DO from := R.Add(from, by); x[j] := from; END;
    END;
    RETURN x;
  END ArithSeq;

(*-----------------*)
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): V.T =
  VAR x := NEW(V.T, num);
  BEGIN
    IF num > 0 THEN
      x[0] := from;
      FOR j := 1 TO num - 1 DO from := R.Mul(from, by); x[j] := from; END;
    END;
    RETURN x;
  END GeomSeq;

(*-----------------*)
BEGIN
END VectorBasic.
