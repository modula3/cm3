GENERIC MODULE MatrixFast(V,R);
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

CONST Module = "MatrixFast.";

(*-----------------*)
PROCEDURE New( 
               m,n:CARDINAL):T =
BEGIN
  RETURN NEW(T,m,n);
END New;
(*-----------------*)
PROCEDURE Copy( 
                mat:T):T =
VAR
  m:=NUMBER(mat^); n:=NUMBER(mat[0]);
  tmp:=NEW(T,m,n);
BEGIN
  tmp^:=mat^;
  RETURN tmp;
END Copy;

(*
(*-----------------*)
PROCEDURE Zero( 
                mat:T)=
(*set all zeros*)
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
BEGIN
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      mat[i,j]:=R.Zero;
    END;
  END;
END Zero;
(*-----------------*)
PROCEDURE One( 
               mat:T) RAISES {Error} =
(*set all zeros except diagonal to 1's*)
<*UNUSED*> <*UNUSED*> CONST ftn = "Midentity";
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
BEGIN
  IF m # n THEN
    RAISE Error(Err.bad_size);
  END;
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      mat[i,j]:=R.Zero;
    END;
  END;
  FOR i:=m1 TO mm DO
    mat[i,i]:=R.One;
  END;
END One;
*)


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize( 
                 mat1,mat2:T) RAISES {Error}=
BEGIN
  IF NUMBER(mat1^)   # NUMBER(mat2^) OR
     NUMBER(mat1[0]) # NUMBER(mat2[0]) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*----------------*)
PROCEDURE Add( 
               mat1,mat2:T):T RAISES {Error} =
(*return mat1+mat2*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Add";
VAR
  m:=NUMBER(mat1^);   m1:=0; mm:=LAST(mat1^);
  n:=NUMBER(mat1[0]); n1:=0; nn:=LAST(mat1[0]);
  tmp:T;
BEGIN
  AssertEqualSize(mat1,mat2);

  tmp:=NEW(T,m,n);
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      tmp[i,j]:= mat1[i,j] + mat2[i,j];
    END;
  END;
  RETURN tmp;
END Add;
(*----------------*)
PROCEDURE Sub( 
               mat1,mat2:T):T RAISES {Error} =
(*return mat1-mat2*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Sub";
VAR
  m:=NUMBER(mat1^);   m1:=0; mm:=LAST(mat1^);
  n:=NUMBER(mat1[0]); n1:=0; nn:=LAST(mat1[0]);
  tmp:T;
BEGIN
  AssertEqualSize(mat1,mat2);

  tmp:=NEW(T,m,n);
  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      tmp[i,j]:= mat1[i,j] - mat2[i,j];
    END;
  END;
  RETURN tmp;
END Sub;
(*----------------*)
PROCEDURE Equal( 
               mat1,mat2:T):BOOLEAN RAISES {Error} =
(*return mat1=mat2*)
(*each is mxn*)
<*UNUSED*> CONST ftn = Module & "Sub";
VAR
  m1:=0; mm:=LAST(mat1^);
  n1:=0; nn:=LAST(mat1[0]);
BEGIN
  AssertEqualSize(mat1,mat2);

  FOR i:=m1 TO mm DO
    FOR j:=n1 TO nn DO
      IF mat1[i,j] # mat2[i,j] THEN
        RETURN FALSE;
      END;
    END;
  END;
  RETURN TRUE;
END Equal;

(*-----------------*)
PROCEDURE Mul( 
               mat1,mat2:T):T RAISES {Error}=
(*return mat1*mat2*)
(* mat1:mxn  mat2:nxp  return:mxp*)
<*UNUSED*> CONST ftn = "Mul";
VAR
  m:=NUMBER(mat1^);   m1:=0; mm:=m-1;
  n:=NUMBER(mat1[0]); n1:=0; nn:=n-1;
  p:=NUMBER(mat2[0]); p1:=0; pp:=p-1;
  tmp:T;
  
BEGIN
  IF NUMBER(mat2^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  tmp:=NEW(T,m,p);
  FOR i:=m1 TO mm DO
    FOR j:=p1 TO pp DO
      tmp[i,j]:=R.Zero;
      FOR k:=n1 TO nn DO
        tmp[i,j]:=tmp[i,j] + mat1[i,k] * mat2[k,j];
      END;
    END;
  END;
  RETURN tmp;
END Mul;
(*-----------------*)

(*----------------*)
PROCEDURE MulV(
               A:T; b:V.T):V.T RAISES {Error} =
(*return c, in A x b=c*)
(*A:mxn, b:nx1, return:mx1*)

<*UNUSED*> CONST ftn = Module & "MulV";
VAR
  m:=NUMBER(A^);   m1:=0; mm:=m-1;
  n:=NUMBER(A[0]); n1:=0; nn:=n-1;
  c:=NEW(V.T,m);
BEGIN
  IF NUMBER(b^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  
  FOR i:=m1 TO mm DO
    c[i]:=R.Zero;
    FOR j:=n1 TO nn DO
      c[i]:=c[i]+b[j]*A[i,j];
    END;
  END;
  RETURN c;
END MulV;

(*-----------------*)
PROCEDURE Transpose( 
                     mat:T):T =
<*UNUSED*> CONST ftn = Module & "mTranspose";
VAR
  m:=NUMBER(mat^);    m1:=0; mm:=m-1;
  n:=NUMBER(mat[0]); n1:=0; nn:=n-1;
  tmp:T;
BEGIN
  tmp:=NEW(T,n,m);
  FOR i:=n1 TO nn DO
    FOR j:=m1 TO mm DO
      tmp[i,j]:=mat[j,i];
    END;
  END;
  RETURN tmp;
END Transpose;

(*-----------------*)
BEGIN
END MatrixFast.
