MODULE TestSLE EXPORTS Test;
(*Copyright (c) 1995,1996 Harry George
Abstract: Test driver for LongRealMatrixDecomposition
          (simultaneous linear equations)

12/13/95  Harry George   Initial version: nr utilities
2/17/96   Harry George   Converted to m3na format

*)
FROM xUtils IMPORT Error;
IMPORT LongRealBasic  AS R,
       LongRealFmtLex AS RF,
       LongRealVectorFast AS V,
       LongRealMatrixFast AS M,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixFmtLex AS MF,
       LongRealMatrixDecomposition AS MD,
       xRNG01,
       Fmt;

CONST Module = "TestSLE.";
<*FATAL ANY*>

(*=====================================*)
TYPE
  M3x3 = ARRAY [0..2] OF ARRAY [0..2] OF R.T;
  V3   = ARRAY [0..2] OF R.T;

VAR
  rand:=NEW(xRNG01.ran1).init();

  (*---must do build_data before using these---*)
  n:=0; n1:=0; nn:=n-1;
  A:M.T;
  B:V.T;
  C:M.T;
  D:M.T;

  knownX:V.T;  (*X is an nx1 matrix*)
  foundX:V.T;

(*---------------------*)
(*---------------------*)
PROCEDURE build_AX(size:CARDINAL:=3)=
<*UNUSED*>
CONST ftn = Module & "build_AX";
BEGIN
  n:=size; n1:=0; nn:=n-1;
  A:=NEW(M.T,n,n);
  C:=NEW(M.T,n,n);
  D:=NEW(M.T,n,n);
  knownX:=NEW(V.T,n);  (*X is an nx1 matrix*)
  foundX:=NEW(V.T,n);

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
PROCEDURE build_B(<*UNUSED*>size:CARDINAL:=3)=
<*UNUSED*>
CONST ftn = Module & "build_B";
BEGIN
  B:=M.MulV(A,knownX);
END build_B;
(*---------------------*)
PROCEDURE build_data(size:CARDINAL:=3)=
<*UNUSED*>
CONST ftn = Module & "build_data";
BEGIN
  build_AX(size);
  build_B(size);
END build_data;
(*--------------------*)
PROCEDURE TestBacksub():BOOLEAN=
CONST
  ftn = Module & "TestBacksub";
VAR
  size:=4;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  build_AX(size);
  (*---zero out lower triangle---*)
  FOR row:=n1 TO nn DO
    FOR col:=n1 TO row-1 DO
      A[row,col]:=R.Zero;
    END;
  END;
  build_B(size);

  Msg("A=" & MF.Fmt(A));
  Msg("B=" & VF.Fmt(B));
  MD.BackSubst(A,x:=foundX,b:=B);
  Msg("knownX=" & VF.Fmt(knownX));
  Msg("foundX=" & VF.Fmt(foundX));
  RETURN result;
END TestBacksub;
(*--------------------*)
PROCEDURE TestHouseholder():BOOLEAN=
CONST
  ftn = Module & "TestHouseholder";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  build_data(4);

  Msg("A=" & MF.Fmt(A));
  MD.HouseHolder(A);
  Msg("HouseHolder(A)=" & MF.Fmt(A));
  RETURN result;
END TestHouseholder;
(*--------------------*)
PROCEDURE TestTridiag():BOOLEAN=
CONST
  ftn = Module & "TestTridiag";
  n = 3; n1=0; nn=n-1;
VAR
  A:=NEW(M.T,n,n);
  knownX:=NEW(R.Array,n);
  foundX:=NEW(R.Array,n);
  a:=NEW(R.Array,n);
  b:=NEW(R.Array,n);
  c:=NEW(R.Array,n);
  r:R.Array;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  A^:=M3x3{V3{1.0d0, 1.0d0, 0.0d0},
           V3{0.5d0, 2.0d0, 1.0d0},
           V3{0.0d0, 0.5d0, 3.0d0}};
  FOR i:=n1 TO nn DO
    IF i>n1 THEN a[i]:=A[i,i-1]; END;
    b[i]:=A[i,i];
    IF i<nn THEN c[i]:=A[i,i+1]; END;
  END;
  Msg("A=" & MF.Fmt(A));
  knownX^:=V3{1.0d0,2.0d0,3.0d0};
  Msg("knownX=" & VF.Fmt(knownX));
  r:=M.MulV(A,knownX);
  Msg("r=     " & VF.Fmt(r));

  MD.SolveTriDiag(a,b,c,r,foundX);
  Msg("foundX=" & VF.Fmt(foundX));
  RETURN result;
END TestTridiag;
(*---------------------*)
(* LU factor           *)
(*---------------------*)
(*------------------------*)
PROCEDURE TestLU():BOOLEAN RAISES {} =
CONST ftn = Module & "TestLU";
VAR
  Acopy:=NEW(M.T,n,n);
  det:R.T;
  d:INTEGER;
  index:=NEW(REF MD.IndexArray,n);
BEGIN
  Debug(1,ftn,"begin\n");

  build_data();
  TRY
    MD.LUFactor(A,index^,d);
    Msg("after LUFactor: d=" & Fmt.Int(d)
      & ", A=" & MF.Fmt(A));
    (*---make a copy so we can reuse the decomp---*)
    Acopy^:=A^;
    det:=MD.LUDet(Acopy,d);
    Msg("det=" & RF.Fmt(det) & "\n");

    Acopy^:=A^;
    D:=MD.LUInverse(A,index^);
    Msg("A inverse =" & MF.Fmt(D));

    Acopy^:=A^;
    foundX^:=B^;
    MD.LUBackSubst(Acopy,foundX,index^);

  EXCEPT
  | Error(err) =>
    EVAL err;
    Msg("LU fails\n");
    RETURN FALSE;
  END;

  (*---report results---*)
  Msg("knownX= " & VF.Fmt(knownX));
  Msg("foundX= " & VF.Fmt(foundX));
  RETURN TRUE;
END TestLU;
(*------------------------*)
PROCEDURE TestSLE():BOOLEAN=
BEGIN
  NewLine(); EVAL TestBacksub();
  NewLine(); EVAL TestHouseholder();
  NewLine(); EVAL TestTridiag();
  NewLine(); EVAL TestLU();
  RETURN TRUE;
END TestSLE;
(*=======================*)
BEGIN
END TestSLE.
