MODULE tMat EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for xMat module.

1/1/96    <name>   Initial version

*)
FROM xUtils IMPORT Error,Err;
IMPORT IO,Wr,Fmt,xReal64 AS R,xVect AS V,xMat AS M;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "tMat.";

(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
(*----------------------*)
PROCEDURE test_matrix():BOOLEAN=
CONST
  ftn = Module & "test_matrix";
  m=3; n=3;
  v3=ARRAY [0..2] OF REAL64 {1.0D0,2.0D0,3.0D0};
VAR
  result:=TRUE;
  m1:=M.new(m,n);
  m2:M.Matrix;
  b:=V.new(n);
BEGIN
  debug(1,ftn,"begin\n");

  m1[0]:=v3; m1[1]:=v3; m1[2]:=v3;
  msg("m1=\n" & M.fmt(m1) & "\n");

  m2:=M.copy(m1);
  msg("m2:=copy(m1)=\n" & M.fmt(m2) & "\n");

  b^:=v3;  b:=M.mulV(m1,b);
  msg("m1*v3=\n" & V.fmt(b) & "\n");

  msg("m1+m2=\n" & M.fmt(M.add(m1,m2)) & "\n");
  msg("m1-m2=\n" & M.fmt(M.sub(m1,m2)) & "\n");
  msg("m1*m2=\n" & M.fmt(M.mul(m1,m2)) & "\n");

  msg("m1^T=\n" & M.fmt(M.transpose(m1)) & "\n");

  RETURN result;   
END test_matrix;
(*-------------------------*)
PROCEDURE test_Mat():BOOLEAN=
CONST ftn = Module & "test_Mat";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_matrix();
  RETURN result;
END test_Mat;
(*=======================*)
BEGIN
END tMat.
