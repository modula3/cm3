MODULE TestMatrix EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for xMat module.

1/1/96    <name>   Initial version

*)
FROM xUtils IMPORT Error,Err;
IMPORT IO,Wr,Fmt,xReal64 AS R,xVect AS V,xMat AS M;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "TestMatrix.";

(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;
END TestABC;
(*----------------------*)
PROCEDURE TestMatrixBasic():BOOLEAN=
CONST
  ftn = Module & "TestMatrixBasic";
  m=3; n=3;
  v3=ARRAY [0..2] OF REAL64 {1.0D0,2.0D0,3.0D0};
VAR
  result:=TRUE;
  m1:=M.new(m,n);
  m2:M.Matrix;
  b:=V.new(n);
BEGIN
  Debug(1,ftn,"begin\n");

  m1[0]:=v3; m1[1]:=v3; m1[2]:=v3;
  Msg("m1=\n" & M.fmt(m1) & "\n");

  m2:=M.copy(m1);
  Msg("m2:=copy(m1)=\n" & M.fmt(m2) & "\n");

  b^:=v3;  b:=M.mulV(m1,b);
  Msg("m1*v3=\n" & V.fmt(b) & "\n");

  Msg("m1+m2=\n" & M.fmt(M.add(m1,m2)) & "\n");
  Msg("m1-m2=\n" & M.fmt(M.sub(m1,m2)) & "\n");
  Msg("m1*m2=\n" & M.fmt(M.mul(m1,m2)) & "\n");

  Msg("m1^T=\n" & M.fmt(M.transpose(m1)) & "\n");

  RETURN result;
END TestMatrixBasic;
(*-------------------------*)
PROCEDURE TestMatrix():BOOLEAN=
CONST ftn = Module & "TestMatrix";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestMatrixBasic();
  RETURN result;
END TestMatrix;
(*=======================*)
BEGIN
END TestMatrix.
