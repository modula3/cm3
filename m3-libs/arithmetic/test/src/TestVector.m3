MODULE tVect EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Vect module.

1/1/96    <name>   Initial version

*)
FROM xUtils IMPORT Error,Err;
IMPORT IO,Wr,Fmt,xReal64 AS R, xVect AS V;
FROM xReal64 IMPORT REAL64;

(*=======================*)
CONST
  Module = "tVect.";

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
PROCEDURE test_vectors():BOOLEAN=
CONST
  ftn = Module & "test_vectors";
  n=4;
VAR
  result:=TRUE;
  v1:=V.new(n);
  v2:=V.new(n);  
BEGIN
  debug(1,ftn,"begin\n");
  V.Zero(v1); msg("zero     =" & V.fmt(v1) & "\n");
 
  v1[0]:=0.0D0; v1[1]:=1.0D0; v1[2]:=2.0D0; v1[3]:=3.0D0;
  msg("v1       =" & V.fmt(v1) & "\n");
  v2:=V.copy(v1); 
  msg("copy(v1) =" & V.fmt(v2) & "\n");
  msg("|v1|     =" & R.fmt(V.abs(v1)) & "\n");

  V.scale(v2,3.0D0);
  msg("v1*3.0   =" & V.fmt(v2) & "\n");

  msg("v2       =" & V.fmt(v2) & "\n");
  msg("v1+v2    =" & V.fmt(V.add(v1,v2)) & "\n");
  msg("v1-v2    =" & V.fmt(V.sub(v1,v2)) & "\n");
  msg("v1 dot v2=" & R.fmt(V.dot(v2,v1)) & "\n");
  TRY
    msg("v1 x v2  =");
    msg(V.fmt(V.cross(v2,v1)) & "\n");
  EXCEPT
  | Error(err) => msg("not implemented\n");
  END;
    
  RETURN result;   
END test_vectors;
(*-------------------------*)
PROCEDURE test_Vect():BOOLEAN=
CONST ftn = Module & "test_Vect";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_vectors();
  RETURN result;
END test_Vect;
(*=======================*)
BEGIN
END tVect.
