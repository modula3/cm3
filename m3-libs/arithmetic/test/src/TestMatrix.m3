MODULE TestMatrix EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for LongRealMatrixBasic module.

1/1/96    <name>   Initial version

*)
(*FROM NADefinitions IMPORT Error,Err;*)
IMPORT (*IO,Wr,Fmt,*)
       LongRealBasic AS R,
       LongRealVectorBasic  AS V,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixBasic  AS M,
       LongRealMatrixFmtLex AS MF;

(*=======================*)
CONST
  Module = "TestMatrix.";

(*----------------------*)
<*FATAL ANY*>
PROCEDURE TestMatrixBasic():BOOLEAN=
CONST
  ftn = Module & "TestMatrixBasic";
  m=3; n=3;
  v3=ARRAY [0..2] OF R.T {1.0D0,2.0D0,3.0D0};
VAR
  result:=TRUE;
  m1:=M.New(m,n);
  m2:M.T;
  b:=V.New(n);
BEGIN
  Debug(1,ftn,"begin\n");

  m1[0]:=v3; m1[1]:=v3; m1[2]:=v3;
  Msg("m1=\n" & MF.Fmt(m1) & "\n");

  m2:=M.Copy(m1);
  Msg("m2:=copy(m1)=\n" & MF.Fmt(m2) & "\n");

  b^:=v3;  b:=M.MulV(m1,b);
  Msg("m1*v3=\n" & VF.Fmt(b) & "\n");

  Msg("m1+m2=\n" & MF.Fmt(M.Add(m1,m2)) & "\n");
  Msg("m1-m2=\n" & MF.Fmt(M.Sub(m1,m2)) & "\n");
  Msg("m1*m2=\n" & MF.Fmt(M.Mul(m1,m2)) & "\n");

  Msg("m1^T=\n" & MF.Fmt(M.Transpose(m1)) & "\n");

  RETURN result;
END TestMatrixBasic;
(*-------------------------*)
PROCEDURE TestMatrix():BOOLEAN=
<*UNUSED*> CONST ftn = Module & "TestMatrix";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestMatrixBasic();
  RETURN result;
END TestMatrix;
(*=======================*)
BEGIN
END TestMatrix.
