MODULE TestVector EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for Vect module.

1/1/96    <name>   Initial version

*)
FROM xUtils IMPORT Error,Err;
IMPORT IO,Wr,Fmt,xReal64 AS R, xVect AS V;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "TestVector.";

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
PROCEDURE TestVectororBasic():BOOLEAN=
CONST
  ftn = Module & "TestVectororBasic";
  n=4;
VAR
  result:=TRUE;
  v1:=V.new(n);
  v2:=V.new(n);  
BEGIN
  Debug(1,ftn,"begin\n");
  V.Zero(v1); Msg("zero     =" & V.fmt(v1) & "\n");
 
  v1[0]:=0.0D0; v1[1]:=1.0D0; v1[2]:=2.0D0; v1[3]:=3.0D0;
  Msg("v1       =" & V.fmt(v1) & "\n");
  v2:=V.copy(v1); 
  Msg("copy(v1) =" & V.fmt(v2) & "\n");
  Msg("|v1|     =" & R.fmt(V.abs(v1)) & "\n");

  V.scale(v2,3.0D0);
  Msg("v1*3.0   =" & V.fmt(v2) & "\n");

  Msg("v2       =" & V.fmt(v2) & "\n");
  Msg("v1+v2    =" & V.fmt(V.add(v1,v2)) & "\n");
  Msg("v1-v2    =" & V.fmt(V.sub(v1,v2)) & "\n");
  Msg("v1 dot v2=" & R.fmt(V.dot(v2,v1)) & "\n");
  TRY
    Msg("v1 x v2  =");
    Msg(V.fmt(V.cross(v2,v1)) & "\n");
  EXCEPT
  | Error(err) => Msg("not implemented\n");
  END;
    
  RETURN result;   
END TestVectororBasic;
(*-------------------------*)
PROCEDURE TestVector():BOOLEAN=
CONST ftn = Module & "TestVector";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestVectororBasic();
  RETURN result;
END TestVector;
(*=======================*)
BEGIN
END TestVector.
