GENERIC MODULE MatrixBasic(V,R);
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
          Converted to OO format and R.T

2/17/96   Harry George   ...and back to ADT format
*)

FROM xUtils IMPORT Error,Err;

CONST Module = "MatrixBasic.";

(*-----------------*)
PROCEDURE New(
               m,n:CARDINAL):T =
BEGIN
  RETURN NEW(T,m,n);
END New;
(*-----------------*)
PROCEDURE Copy(
                x:T):T =
VAR
  m:=NUMBER(x^);
  n:=NUMBER(x[0]);
  z:=NEW(T,m,n);
BEGIN
  z^:=x^;
  RETURN z;
END Copy;

(*-----------------*)
PROCEDURE NewZero(m,n:CARDINAL):T =               (*create zero matrix*)
VAR
  mf:=0; ml:=m-1;
  nf:=0; nl:=n-1;
  z:=NEW(T,m,n);
BEGIN
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      z[i,j]:=R.Zero;
    END;
  END;
  RETURN z;
END NewZero;
(*-----------------*)
PROCEDURE NewOne (n  :CARDINAL):T =               (*create identity matrix*)
VAR
  nf:=0; nl:=n-1;
  z:=NEW(T,n,n);
BEGIN
  FOR i:=nf TO nl DO
    z[i,i]:=R.One;
    FOR j:=nf TO i-1 DO
      z[i,j]:=R.Zero;
      z[j,i]:=R.Zero;
    END;
  END;
  RETURN z;
END NewOne;


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize(
                 x,y:T) RAISES {Error}=
BEGIN
  IF NUMBER(x^)   # NUMBER(y^) OR
     NUMBER(x[0]) # NUMBER(y[0]) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*----------------*)
PROCEDURE Add(
               x,y:T):T RAISES {Error} =
(*return x+y*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Add";
VAR
  m:=NUMBER(x^);   mf:=0; ml:=LAST(x^);
  n:=NUMBER(x[0]); nf:=0; nl:=LAST(x[0]);
  z:T;
BEGIN
  AssertEqualSize(x,y);

  z:=NEW(T,m,n);
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      z[i,j]:= R.Add(x[i,j], y[i,j]);
    END;
  END;
  RETURN z;
END Add;
(*----------------*)
PROCEDURE Sub(
               x,y:T):T RAISES {Error} =
(*return x-y*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Sub";
VAR
  m:=NUMBER(x^);   mf:=0; ml:=LAST(x^);
  n:=NUMBER(x[0]); nf:=0; nl:=LAST(x[0]);
  z:T;
BEGIN
  AssertEqualSize(x,y);

  z:=NEW(T,m,n);
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      z[i,j] := R.Sub(x[i,j], y[i,j]);
    END;
  END;
  RETURN z;
END Sub;
(*----------------*)
PROCEDURE IsZero(
               x:T):BOOLEAN =
VAR
  mf:=0; ml:=LAST(x^);
  nf:=0; nl:=LAST(x[0]);
BEGIN
  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      IF NOT R.IsZero (x[i,j]) THEN
        RETURN FALSE;
      END;
    END;
  END;
  RETURN TRUE;
END IsZero;

(*----------------*)
PROCEDURE Equal(
               x,y:T):BOOLEAN RAISES {Error} =
(*return x=y*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Equal";
VAR
  mf:=0; ml:=LAST(x^);
  nf:=0; nl:=LAST(x[0]);
BEGIN
  AssertEqualSize(x,y);

  FOR i:=mf TO ml DO
    FOR j:=nf TO nl DO
      IF NOT R.Equal (x[i,j], y[i,j]) THEN
        RETURN FALSE;
      END;
    END;
  END;
  RETURN TRUE;
END Equal;

(*-----------------*)
PROCEDURE Mul(
               x,y:T):T RAISES {Error}=
(*return x*y*)
(* x:mxn  y:nxp  return:mxp*)
<*UNUSED*> CONST ftn = "Mul";
VAR
  m:=NUMBER(x^);   mf:=0; ml:=m-1;
  n:=NUMBER(x[0]); nf:=0; nl:=n-1;
  p:=NUMBER(y[0]); pf:=0; pl:=p-1;
  z:T;
  sum:R.T;

BEGIN
  IF NUMBER(y^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  z:=NEW(T,m,p);
  FOR i:=mf TO ml DO
    FOR j:=pf TO pl DO
      sum:=R.Zero;
      FOR k:=nf TO nl DO
        sum:=R.Add(sum, R.Mul(x[i,k], y[k,j]));
      END;
      z[i,j]:=sum;
    END;
  END;
  RETURN z;
END Mul;
(*-----------------*)

PROCEDURE MulV(
               A:T; b:V.T):V.T RAISES {Error} =
(*return c, in A x b=c*)
(*A:mxn, b:nx1, return:mx1*)

<*UNUSED*> CONST ftn = Module & "MulV";
VAR
  m:=NUMBER(A^);   mf:=0; ml:=m-1;
  n:=NUMBER(A[0]); nf:=0; nl:=n-1;
  c:=NEW(V.T,m);
  sum:R.T;
BEGIN
  IF NUMBER(b^)#n THEN
    RAISE Error(Err.bad_size);
  END;

  FOR i:=mf TO ml DO
    sum:=R.Zero;
    FOR j:=nf TO nl DO
      sum:=R.Add(sum,R.Mul(b[j],A[i,j]));
    END;
    c[i]:=sum;
  END;
  RETURN c;
END MulV;

(*-----------------*)
PROCEDURE Transpose(
                     x:T):T =
<*UNUSED*> CONST ftn = Module & "Transpose";
VAR
  m:=NUMBER(x^);   mf:=0; ml:=m-1;
  n:=NUMBER(x[0]); nf:=0; nl:=n-1;
  z:T;
BEGIN
  z:=NEW(T,n,m);
  FOR i:=nf TO nl DO
    FOR j:=mf TO ml DO
      z[i,j]:=x[j,i];
    END;
  END;
  RETURN z;
END Transpose;

(*-----------------*)
PROCEDURE Adjungate(
                     x:T):T =
<*UNUSED*> CONST ftn = Module & "Adjungate";
VAR
  m:=NUMBER(x^);   mf:=0; ml:=m-1;
  n:=NUMBER(x[0]); nf:=0; nl:=n-1;
  z:T;
BEGIN
  z:=NEW(T,n,m);
  FOR i:=nf TO nl DO
    FOR j:=mf TO ml DO
      z[i,j]:=R.Conj(x[j,i]);
    END;
  END;
  RETURN z;
END Adjungate;

(*-----------------*)
BEGIN
END MatrixBasic.
