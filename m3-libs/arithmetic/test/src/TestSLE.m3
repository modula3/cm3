MODULE tSLE EXPORTS test;
(*Copyright (c) 1995,1996 Harry George
Abstract: Test driver for xSLE (simultaneous
          linear equations)

12/13/95  Harry George   Initial version: nr utilities
2/17/96   Harry George   Converted to m3na format

*)
FROM xUtils IMPORT Error,Err;
IMPORT xInteger AS I,
       xReal64 AS R,
       xVect AS V,
       xMat AS M,
       xRNG01;
FROM xReal64 IMPORT REAL64;
IMPORT xSLE;

CONST Module = "tSLE.";

(*=====================================*)
TYPE
  M3x3 = ARRAY [0..2] OF ARRAY [0..2] OF REAL64;
  V3   = ARRAY [0..2] OF REAL64;
  
VAR
  rand:=NEW(xRNG01.ran1).init();

  (*---must do build_data before using these---*)
  n:=0; n1:=0; nn:=n-1;
  A:M.Matrix; 
  B:V.Vector;
  C:M.Matrix;   
  D:M.Matrix; 

  knownX:V.Vector;  (*X is an nx1 matrix*)
  foundX:V.Vector;

(*---------------------*)
(*---------------------*)
PROCEDURE build_AX(size:CARDINAL:=3)=
CONST ftn = Module & "build_AX";
BEGIN
  n:=size; n1:=0; nn:=n-1;
  A:=NEW(M.Matrix,n,n); 
  C:=NEW(M.Matrix,n,n);   
  D:=NEW(M.Matrix,n,n); 
  knownX:=NEW(V.Vector,n);  (*X is an nx1 matrix*)
  foundX:=NEW(V.Vector,n);

  FOR i:=n1 TO nn DO
    FOR j:=n1 TO nn DO
      A[i,j]:=rand.uniform(0.0d0,9.99d0);
    END;
  END (*for*);

  FOR i:=n1 TO nn DO
    knownX[i]:=rand.uniform(0.0d0,9.99d0);
  END (*for*);

END build_AX;
(*---------------------*)
PROCEDURE build_B(size:CARDINAL:=3)=
CONST ftn = Module & "build_B";
BEGIN
  B:=M.mulV(A,knownX);
END build_B;
(*---------------------*)
PROCEDURE build_data(size:CARDINAL:=3)=
CONST ftn = Module & "build_data";
BEGIN
  build_AX(size);
  build_B(size);
END build_data;
(*--------------------*)
PROCEDURE test_backsub():BOOLEAN=
CONST
  ftn = Module & "test_backsub";
VAR
  size:=4;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  build_AX(size);
  (*---zero out lower triangle---*)
  FOR row:=n1 TO nn DO
    FOR col:=n1 TO row-1 DO
      A[row,col]:=R.Zero;
    END;
  END;
  build_B(size);

  msg("A=" & M.fmt(A)); 
  msg("B=" & V.fmt(B));
  xSLE.backsub(A,x:=foundX,b:=B);
  msg("knownX=" & V.fmt(knownX)); 
  msg("foundX=" & V.fmt(foundX)); 
  RETURN result;  
END test_backsub;  
(*--------------------*)
PROCEDURE test_householder():BOOLEAN=
CONST
  ftn = Module & "test_householder";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  build_data(4);

  msg("A=" & M.fmt(A)); 
  xSLE.householder(A);
  msg("householder(A)=" & M.fmt(A)); 
  RETURN result;  
END test_householder;  
(*--------------------*)
PROCEDURE test_tridiag():BOOLEAN=
CONST
  ftn = Module & "test_tridiag";
  n = 3; n1=0; nn=n-1;
VAR
  A:=NEW(M.Matrix,n,n);
  knownX:=NEW(R.Array,n);
  foundX:=NEW(R.Array,n);
  a:=NEW(R.Array,n);
  b:=NEW(R.Array,n);
  c:=NEW(R.Array,n);
  u:=NEW(R.Array,n);
  r:R.Array;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  A^:=M3x3{V3{1.0d0, 1.0d0, 0.0d0},
           V3{0.5d0, 2.0d0, 1.0d0},
           V3{0.0d0, 0.5d0, 3.0d0}};
  FOR i:=n1 TO nn DO
    IF i>n1 THEN a[i]:=A[i,i-1]; END;
    b[i]:=A[i,i];
    IF i<nn THEN c[i]:=A[i,i+1]; END;    
  END;
  msg("A=" & M.fmt(A));
  knownX^:=V3{1.0d0,2.0d0,3.0d0};
  msg("knownX=" & V.fmt(knownX));
  r:=M.mulV(A,knownX);
  msg("r=     " & V.fmt(r));
  
  xSLE.tridiag(a,b,c,r,foundX);
  msg("foundX=" & V.fmt(foundX));
  RETURN result;  
END test_tridiag;  
(*---------------------*)
(* LU factor           *)
(*---------------------*)
(*------------------------*)
PROCEDURE test_LU():BOOLEAN RAISES {} =
CONST ftn = Module & "test_LU";
VAR
  Acopy:=NEW(M.Matrix,n,n);
  det:REAL64;
  d:INTEGER;
  index:=NEW(I.Array,n);
BEGIN
  debug(1,ftn,"begin\n");
  
  build_data();
  TRY
    xSLE.LUfactor(A,index,d);
    msg("after LUfactor: d=" & I.fmt(d)
      & ", A=" & M.fmt(A));
    (*---make a copy so we can reuse the decomp---*)   
    Acopy^:=A^;
    det:=xSLE.LUdet(Acopy,d);
    msg("det=" & R.fmt(det) & "\n");

    Acopy^:=A^;
    xSLE.LUinverse(A,D,index);
    msg("A inverse =" & M.fmt(D));
        
    Acopy^:=A^;
    foundX^:=B^;
    xSLE.LUbacksub(Acopy,foundX,index);

  EXCEPT
  | Error(code) => msg("LU fails\n");
                   RETURN FALSE;
  END;

  (*---report results---*)
  msg("knownX= " & V.fmt(knownX));
  msg("foundX= " & V.fmt(foundX));
  RETURN TRUE;
END test_LU;
(*------------------------*)
PROCEDURE test_SLE():BOOLEAN=
BEGIN
  newline(); EVAL test_backsub();
  newline(); EVAL test_householder();
  newline(); EVAL test_tridiag();
  newline(); EVAL test_LU();
  RETURN TRUE;
END test_SLE;
(*=======================*)
BEGIN
END tSLE.
