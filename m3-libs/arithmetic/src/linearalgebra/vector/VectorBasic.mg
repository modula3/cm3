GENERIC MODULE VectorBasic(R,VS);
(*
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
          Converted to OO format, and R.T

2/17/96   Harry George   Converted from OO to ADT format
*)
FROM NADefinitions IMPORT Error,Err;

<*UNUSED*> CONST Module = "VectorBasic.";

(*-----------------*)
PROCEDURE New(  n:CARDINAL):T =
BEGIN
  RETURN NEW(T,n);
END New;
(*-----------------*)
PROCEDURE FromArray(READONLY x:TBody):T =
VAR
  n:=NUMBER(x);
  z:=NEW(T,n);
BEGIN
  z^:=x;
  RETURN z;
END FromArray;
(*-----------------*)
PROCEDURE Copy(  x:T):T =
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  z^:=x^;
  RETURN z;
END Copy;


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize(x,y:T) RAISES {Error}=
BEGIN
  IF NUMBER(x^) # NUMBER(y^) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*---------------------*)
PROCEDURE IsZero(x:T):BOOLEAN =
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    IF NOT R.IsZero(x[i]) THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END IsZero;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error} =
BEGIN
  AssertEqualSize(x,y);
  FOR i:=FIRST(x^) TO LAST(x^) DO
    IF NOT R.Equal(x[i],y[i]) THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END Equal;

(*-----------------*)
PROCEDURE Add(x,y:T):T RAISES {Error}=
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  VS.Add(z^,x^,y^);
  RETURN z;
END Add;

(*-----------------*)
PROCEDURE Sub(x,y:T):T RAISES {Error}=
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  VS.Sub(z^,x^,y^);
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Neg(x:T):T =    (*return -x *)
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  TRY
    VS.Neg(y^,x^);
  EXCEPT
  | Error(err) => EVAL err; <*ASSERT FALSE*>
  END;
  RETURN y;
END Neg;


(*-----------------*)
PROCEDURE Scale(
                 x:T; y:R.T):T=
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  TRY
    VS.Scale(z^,x^,y);
  EXCEPT
  | Error(err) => EVAL err; <*ASSERT FALSE*>
  END;
  RETURN z;
END Scale;


(*-----------------*)
PROCEDURE Inner(
                x,y:T):R.T RAISES {Error}=
BEGIN
  RETURN VS.Inner(x^,y^);
END Inner;

(*-----------------*)
(* should be generalized to finding an orthonormal basis
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
                x,y:T):T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
*)

(*-----------------*)
PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): T =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := R.Add(from, by); END;
    END;
    RETURN x;
  END ArithSeq;

(*-----------------*)
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): T =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := R.Mul(from, by); END;
    END;
    RETURN x;
  END GeomSeq;

(*-----------------*)
PROCEDURE RecursiveSeq (num: CARDINAL; from: R.T; by: VS.MapFtn): T =
  VAR x := NEW(T, num);
  BEGIN
    FOR j := 0 TO num - 1 DO
      x[j] := from;
      IF j < num - 1 THEN from := by(from); END;
    END;
    RETURN x;
  END RecursiveSeq;

(*-----------------*)
BEGIN
END VectorBasic.
