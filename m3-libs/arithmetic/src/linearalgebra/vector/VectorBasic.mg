GENERIC MODULE VectorBasic(R);
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
  n:=NUMBER(x^);
  z:=NEW(T,n);
BEGIN
  z^:=x^;
  RETURN z;
END Copy;


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize(
                 x,y:T) RAISES {Error}=
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
PROCEDURE Add(
                 x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  AssertEqualSize(x,y);
  z:=NEW(T,NUMBER(x^));
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=R.Add(x[i],y[i]);
  END;
  RETURN z;
END Add;

(*-----------------*)
PROCEDURE Sub(
               x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  AssertEqualSize(x,y);
  z:=NEW(T,NUMBER(x^));
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=R.Sub(x[i],y[i]);
  END;
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Neg(x:T):T =    (*return -x *)
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    y[i] := R.Neg(x[i]);
  END;
  RETURN y;
END Neg;


(*-----------------*)
PROCEDURE Scale(
                 x:T; y:R.T):T=
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=R.Mul(x[i],y);
  END;
  RETURN z;
END Scale;


(*-----------------*)
PROCEDURE Inner(
                x,y:T):R.T RAISES {Error}=
VAR
  sum:R.T;
BEGIN
  AssertEqualSize(x,y);
  sum:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    sum:=R.Add(sum,R.Mul(R.Conj(x[i]),y[i]));
  END;
  RETURN sum;
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

PROCEDURE Apply(x:T;f:ApplyFtn)=
  BEGIN
    FOR j:=0 TO LAST(x^) DO
      f(x[j]);
    END;
  END Apply;

PROCEDURE Map(x:T;f:MapFtn):T=
  VAR
    y:=NEW(T,NUMBER(x^));
  BEGIN
    FOR j:=0 TO LAST(x^) DO
      y[j]:=f(x[j]);
    END;
    RETURN y;
  END Map;

PROCEDURE Reduce(x:T;f:ReduceFtn;accu:R.T):R.T=
  BEGIN
    FOR j:=0 TO LAST(x^) DO
      accu:=f(accu,x[j]);
    END;
    RETURN accu;
  END Reduce;


PROCEDURE Sum(READONLY x:TBody):R.T=
  VAR
    sum:=R.Zero;
  BEGIN
    FOR i:=FIRST(x) TO LAST(x) DO
      sum:=R.Add(sum,x[i]);
    END;
    RETURN sum;
  END Sum;

(*-----------------*)
BEGIN
END VectorBasic.
