GENERIC MODULE MatrixDecomposition(M,V,R,RT);
(*
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic SLErices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL64.

1/27/96   Harry George
          Converted to OO format

2/17/96   Harry George   ...and back to ADT with REAL64.
*)
FROM xUtils IMPORT Error,Err;
FROM RT IMPORT Tiny, Eps;

CONST Module = "MatrixDecomposition";

(*==========================*)
(* Triangluar Matrices      *)
(*==========================*)
(*A triangular matrix A is of the form:
| a11 a12 a13 a14
| 0   a22 a23 a24
| 0   0   a33 a34
| 0   0   0   a44

A x = b can be solved for b by back substitution
*)
PROCEDURE backsub(A:M.T;
                  x,b:V.T
                  ) RAISES {Error}=

VAR
  m:=NUMBER(A^);    m1:=FIRST(A^);   mm:=LAST(A^);
  n:=NUMBER(A[0]);
  tmp:R.T;
BEGIN
  IF (m#n) OR (NUMBER(x^)#n) OR (NUMBER(b^)#n) THEN
    RAISE Error(Err.bad_size);
  END;

  FOR row:=mm TO m1 BY -1 DO
    IF ABS(b[row])<Tiny THEN RAISE Error(Err.divide_by_zero); END;
    tmp:=b[row];
    FOR col:=row+1 TO mm DO
      tmp:=tmp-A[row,col]*x[col];
    END;
    x[row]:=tmp/A[row,row];
  END;
END backsub;
(*==========================*)
(* Tridiagonal Matrices     *)
(*==========================*)
(*------------------------*)
PROCEDURE householder(A:M.T) RAISES{Error}=   (*nxn*)
(*Convert A to tridiagonal form (destroying original A)*)
VAR
  n:=NUMBER(A^);    n1:=FIRST(A^);    nn:=LAST(A^);
  m:=NUMBER(A[0]);
  u:=NEW(V.T,n);
  t:=NEW(M.T,n,n);
  sum,rootsum,w,h,uau,b23:R.T;
BEGIN
  IF n#m THEN RAISE Error(Err.bad_size); END;

  FOR row:=n1 TO nn-2 DO
    sum:=R.Zero;
    FOR i:=n1 TO nn DO
      u[i]:=R.Zero;
      IF i > row+1 THEN u[i]:=A[i,row]; END;
      IF i > row   THEN sum:=sum+A[i,row]*A[i,row]; END;
    END;
    w:=R.One;
    IF A[row+1,row] < R.Zero THEN w:=-R.One; END;
    rootsum:=RT.SqRt(sum);
    h:=sum+ABS(A[row+1,row])*rootsum;
    u[row+1]:=A[row+1,row]+rootsum*w;
    uau:=R.Zero;
    FOR i:=n1 TO nn DO
      FOR j:=n1 TO nn DO
        uau:=uau+u[i]*A[i,j]*u[j];
        IF (i <= row) AND (j <= row) THEN
          t[i,j]:=A[i,j];
        ELSIF (j = row) AND (i >= row+2) THEN
          t[i,j]:=R.Zero;
        ELSE
          b23:=R.Zero;
          FOR k:=n1 TO nn DO
            b23:=b23 - (u[i]*A[k,j]+A[i,k]*u[j])*u[k];
          END;
          t[i,j]:=A[i,j]+b23/h;
        END;
      END; (*for j*)
    END; (*for i*)
    uau:=uau/h/h;
    FOR i:=n1 TO nn DO
      FOR j:=n1 TO nn DO
        A[i,j]:=t[i,j]+uau*u[i]*u[j];
        IF ABS(A[i,j]) < Eps THEN A[i,j]:=R.Zero; END;
      END;
    END;
  END; (*for row*)
END householder;

(*---------------------*)
PROCEDURE matrix_to_arrays(A:M.T;        (*nxn tridiagonal*)
                           VAR a,b,c:V.T  (*array form*)
                          ) RAISES {Error}=

VAR
  n:=NUMBER(A^);    n1:=FIRST(A^);    nn:=LAST(A^);
  m:=NUMBER(A[0]);
BEGIN
  IF n#m THEN RAISE Error(Err.bad_size); END;

  IF a=NIL THEN a:=NEW(V.T,n); END;
  IF b=NIL THEN b:=NEW(V.T,n); END;
  IF c=NIL THEN c:=NEW(V.T,n); END;

  a[n1]:=R.Zero; b[n1]:=A[n1,n1]; c[n1]:=A[n1,n1+1];
  FOR i:=n1+1 TO nn-1 DO
    a[i]:=A[i,i-1];
    b[i]:=A[i,i];
    c[i]:=A[i,i+1];
  END;
  a[nn]:=A[nn,nn-1]; b[nn]:=A[nn,nn]; c[nn]:=R.Zero;

END matrix_to_arrays;
(*-----------------------*)
PROCEDURE tridiag(a,b,c,r:V.T;
                  VAR u:V.T) RAISES {Error}=
(*Given tridiagonal matrix A, with diagonals a,b,c:
|  b1 c1  0    ...
|  a2 b2 c2    ...
|   0 a3 b3 c3 ...
|              ...
|                 aN-1 bN-1 cN-1
|                  0   aN   bN
|  Solve for u in A*u=r
*)
CONST
  ftn = Module & "tridiag";
VAR
  den:R.T;
  n:=NUMBER(r^); n1:=FIRST(r^); nn:=LAST(r^);
  d:=NEW(V.T,n);
BEGIN
  (*---check preconditions---*)
  IF NUMBER(a^)#n OR NUMBER(b^)#n OR NUMBER(c^)#n THEN
    RAISE Error(Err.bad_size);
  END;
  IF ABS(b[n1]) < Tiny THEN
    RAISE Error(Err.b1_too_small);
  END;

  (*---first row---*)
  den:=b[n1];
  u[n1]:=r[n1]/den;
  d[n1]:=c[n1]/den;

  (*---work forward---*)
  FOR i:=n1+1 TO nn-1 DO
    den:=b[i]-a[i]*d[i-1];
    IF ABS(den) < Tiny THEN
      RAISE Error(Err.divide_by_zero);
    END;
    u[i]:=(r[i]-a[i]*u[i-1])/den;
    d[i]:=c[i]/den;
  END;

  (*---last row---*)
  den:=b[nn]-a[nn]*d[nn-1];
  u[nn]:=(r[nn]-a[nn]*u[nn-1])/den;

  (*---work backward---*)
  FOR i:=nn-1 TO n1 BY -1 DO
    u[i]:=u[i]-d[i]*u[i+1];
  END;
END tridiag;
(*==========================*)
(* nxn Matrices             *)
(*==========================*)
(*A general nxn real matrix A is of the form
| a11 a12 a13
| a21 a22 a23
| a31 a32 a33

A x = b can be solved for x by Gaussian Elimination and
backsubstitution
*)
(*
PROCEDURE GaussElim(A:  M.T;
                    x,b:V.T;
                    pivot:BOOLEAN:=TRUE
                    ) RAISES {Error}=
(*Generally, we need to pivot to assure division by the largest
coeff.  However, sometimes we already know the matrix is in
the correct form and can avoid pivoting.  In that case, set
pivot:=FALSE
*)
VAR
  m:=NUMBER(A^);    m1:=FIRST(A^);   mm:=LAST(A^);
  n:=NUMBER(A[0]);  n1:=FIRST(A[0]); nn:=LAST(A[0]);
  tmp:R.T;
  pndx:CARDINAL;
BEGIN
  IF (m#n) OR (NUMBER(x^)#n) OR (NUMBER(b^)#n) THEN
    RAISE Error(Err.bad_size);
  END;

  FOR row:=n1 TO nn-1 DO
    IF pivot THEN
      (*---look for max scale---*)
      rmax:=row; max:=ABS(x[row]/A[row,row]);
      FOR col:=row TO nn DO
        IF ABS(A[row,col]) >  max THEN
          rmax:=col; max:=ABS(x[row]/A[row,col]);
        END;
      END;
      (*---pivot---*)
      tmprow^:=A[

END GaussElim;
*)

(*-----------------*)
(* LU factoring    *)
(*-----------------*)
PROCEDURE LUfactor(A      :M.T;
               VAR index  :IndexArray;
               VAR d      :INTEGER) RAISES {Error} =
(*Factor A into Lower/Upper portions
Destroys A's values.
A is real nxn
index is integer nx1
return value "d" is used for backsub and det
*)
CONST ftn = "LUfactor";
VAR
  imax:=0;
  sum,dum,max,tmp:R.T;
  Af:=FIRST(A^); Al:=LAST(A^);
  m1:=LAST(A^);   (*num rows*)
  n1:=LAST(A[0]); (*num cols*)
  n2:=LAST(index);
  scale:= NEW(V.T,n1+1);
  tmprow:=NEW(V.T,n1+1);
BEGIN
  IF (m1 # n1) OR (m1 # n2) THEN
    RAISE Error(Err.bad_size);
  END;

  (*---track the row switching parity via d---*)
  d:=1;

  (*---find max for scaling in each row---*)
  FOR i:=Af TO Al DO
    max:=R.Zero;
    FOR j:=Af TO Al DO
      tmp:=ABS(A[i,j]);
      IF tmp > max THEN
        max:=tmp;
      END (* if *);
    END (* for *);
    IF  max=R.Zero THEN
      RAISE Error(Err.divide_by_zero);
    ELSE
      scale[i]:= R.One / max;
    END (* if *);
  END (* for *);

  (*---loop over columns---*)
  FOR j:=Af TO Al DO
   (*---compute beta---*)
   IF j > Af  THEN
      FOR i:=Af TO j-1 DO
        sum:=A[i,j];
        IF i > Af THEN
          FOR k:= Af TO i-1 DO
            sum:=sum - A[i,k] * A[k,j];
          END (* for *);
          A[i,j]:=sum;
        END (* if *);
      END (* for *);
    END (* if *);

    (*---compute alpha---*)
    max:=R.Zero;
    FOR i:=j TO Al DO
      sum:=A[i,j];
      IF j> Af THEN
        FOR k:=Af TO j-1 DO
          sum:= sum - A[i,k] * A[k,j];
        END (* for *);
        A[i,j]:=sum;
      END (* if *);

      (*---is this a better pivot?---*)
      dum:=scale[i] * ABS(sum);
      IF dum > max THEN
        imax:=i;
        max:=dum;
      END (* if *);
    END (* for j to n*);

    (*---exchange rows?---*)
    IF j # imax THEN
      (*swap rows*)
      tmprow^:=A[imax];
      A[imax]:=A[j];
      A[j]:=tmprow^;
      d:=-d;  (*fix parity*)
      scale[imax]:=scale[j];  (*fix scale*)
    END (* if *);

    (*---set the index for this row---*)
    index[j]:=imax;

    (*---divide by pivot---*)
    IF j # Al THEN
      IF A[j,j]=R.Zero THEN
        A[j,j]:=Tiny;
      END (* if *);
      dum:=R.One / A[j,j];
      FOR i:=j+1 TO Al DO
        A[i,j]:=A[i,j] * dum;
      END (* for *);
    END (* if *);
  END (* for next column*);

  (*---last item---*)
  IF A[Al,Al]=R.Zero THEN
    A[Al,Al]:=Tiny;
  END (* if *);
END LUfactor;

(*-----------------*)
PROCEDURE LUbacksub(A     :M.T;
                    B     :V.T;
           READONLY index :IndexArray) RAISES ANY=
(*After LUfactor on A, solves A dot X = B.
X is returned in B.  B's values are destroyed
A is real nxn
B is real nx1
index is integer nx1
*)
CONST ftn = "LUbacksub";
VAR
  Af:=FIRST(A^); Al:=LAST(A^);
  m1:=LAST(A^);   (*num rows*)
  n1:=LAST(A[0]); (*num cols*)
  m2:=LAST(B^);   (*num rows*)
  ii,ip:INTEGER;
  sum:R.T;
BEGIN
  IF (m1 # n1) OR (m2 # m1) THEN
    RAISE Error(Err.bad_size);
  END;

  (*---find first non-zero---*)
  ii:=Af - 1;  (*marker for first non-zero coeff*)
  FOR i:=Af TO Al DO
    ip:=index[i];
    sum:=B[ip];
    B[ip]:=B[i];
    IF ii # Af - 1 THEN
      FOR j:=ii TO i-1 DO
        sum:=sum - A[i,j] * B[j];
      END (* for *);
    ELSIF NOT sum=R.Zero THEN
      ii:=i;
    END (* if *);
    B[i]:=sum;
  END (* for *);

  (*---work through on column basis---*)
  FOR i:= Al TO Af BY -1 DO
    sum:=B[i];
    IF i< Al THEN
      FOR j:=i+1 TO Al DO
        sum:=sum - A[i,j] * B[j];
      END (* for *);
    END (* if *);
    B[i]:=sum / A[i,i];
  END (* for *);
END LUbacksub;
(*-----------------*)
PROCEDURE LUinverse(A    :M.T;
           READONLY index:IndexArray):M.T RAISES ANY=
(*
Inverse of A goes to B
Must have done LUfactor on A first
Destroys A's values.
A is real nxn
B is real nxn
index is integer nx1
*)
CONST ftn = "LUinverse";
VAR
  n:=NUMBER(A^);
  B:M.T;
  C:V.T;
BEGIN
  IF (n # NUMBER(A[0])) OR
     (n # NUMBER(B^))   OR
     (n # NUMBER(B[0])) THEN
    RAISE Error(Err.bad_size);
  END;

  B:=M.NewOne(n);
  (*we need C as a column vector from B*)
  C:=NEW(V.T,n);

  FOR i:=0 TO LAST(B^) DO
    C^:=B[i];
    LUbacksub(A,C,index);
    B[i]:=C^;
(*
    FOR j:=0 TO LAST(C^) DO C[j]:=B[i,j]; END;
    LUbacksub(A,C,index);
    FOR j:=0 TO LAST(C^) DO B[i,j]:=C[j]; END;
*)
  END (* for *);
  RETURN B;
END LUinverse;
(*-----------------*)
PROCEDURE LUdet(A:M.T;
                d:INTEGER):R.T =
(*after LUfactor on A and no backsubs,
returns determinant
"d" is the parity marker from LUfactor
*)
CONST ftn = "LUdet";
VAR
  m:=LAST(A^);   (*num rows*)
  n:=LAST(A[0]); (*num cols*)
  tmp:R.T;
BEGIN
  (*---could do more checking here to assure LU form---*)
  (*---set sign due to row switching---*)
  tmp:=FLOAT(d,R.T);

  (*---compute value---*)
  FOR i:=0 TO n DO
    tmp:=tmp * A[i,i];
  END (* for *);
  RETURN tmp;
END LUdet;

(*=============================*)
(* Singular Value Decomposition*)
(*=============================*)

(*----------------------*)
PROCEDURE svd_golub(
           A:M.T;         (*mxn matrix*)
           b:V.T;         (*nx1 col matrix for each set of *)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.T  (*decomposition products*)
           ) RAISES {Error}=
(*Do SVD via Golub and Reinsch *)
BEGIN

END svd_golub;

(*------------------------*)
PROCEDURE svd_chan(
           A:M.T;         (*mxn matrix*)
           b:V.T;         (*nx1 col matrix*)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.T  (*decomposition products*)
           ) RAISES {Error}=
(*Do SVD via T. Chan's ACM algorithm 581*)
BEGIN
END svd_chan;

(*-----------------------*)
PROCEDURE svd_solve(U,V,W:M.T; (*decomposition*)
                    b:V.T;     (*rightside*)
                    VAR x:V.T  (*result*)
                   ) RAISES {Error}=


BEGIN
END svd_solve;
(*-----------------*)
BEGIN
END MatrixDecomposition.
