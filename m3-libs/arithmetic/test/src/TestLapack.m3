UNSAFE MODULE TestLapack EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)

IMPORT Fmt;
IMPORT LongRealBasic          AS R,
       LongRealVectorBasic    AS V,
       LongRealVectorFmtLex   AS VF,
       LongRealMatrixLapack   AS ML,
       LongRealMatrixFmtLex   AS MF,
       LongRealCharPolynomial AS MCP;

(*=======================*)
CONST
  Module = "TestLapack.";

(*----------------------*)
PROCEDURE TestBasic():BOOLEAN=
CONST
  ftn = Module & "TestBasic";
CONST
  id = ARRAY OF CHAR {'e', 's' , 'b', 'p', 'n', 'r', 'm', 'u', 'l', 'o'};

VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR j:=FIRST(id) TO LAST(id) DO
    Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT
         {Fmt.Char(id[j]), Fmt.LongReal(GetMachineParameter(id[j]))}));
  END;

  RETURN result;
END TestBasic;
(*----------------------*)
PROCEDURE TestLinAlg():BOOLEAN=
CONST
  ftn = Module & "TestLinAlg";

CONST
  size = 3;

VAR
  result:=TRUE;
  (*A:=MCP.CompanionMatrix(V.FromArray(ARRAY OF R.T{1.0D0,3.0D0,3.0D0,1.0D0}));*)
  A:=MCP.CompanionMatrix(V.FromArray(ARRAY OF R.T{-1.0D0,0.0D0,0.0D0,1.0D0}));
  (*
  A:=NEW(REF ARRAY OF ARRAY OF LONGREAL,3,3);
  *)
  Am:=NEW(REF ARRAY OF LONGREAL,9);
  eigRe:=V.New(3);
  eigIm:=V.New(3);
  sdim:INTEGER;
  worksize:=MAX(1,3*size);
  work:=V.New(worksize);
  vs:=ARRAY [0..-1] OF ARRAY [0..-1] OF LONGREAL{};
  bwork:=ARRAY [0..-1] OF BOOLEAN{};
  success:INTEGER;
BEGIN
  Debug(1,ftn,"begin\n");

  SUBARRAY(Am^,0,3) := A[0];
  SUBARRAY(Am^,3,3) := A[1];
  SUBARRAY(Am^,6,3) := A[2];

  Msg(VF.Fmt(Am) & "\n");
  Msg(MF.Fmt(A) & "\n");

  Msg(Fmt.Int(ADR(A^)-LOOPHOLE(A,ADDRESS)) & "\n");
  Msg(Fmt.Int(ADR(A[0])-ADR(A)) & "\n");
  Msg(Fmt.Int(ADR(A[0])-ADR(A^)) & "\n");
  Msg(Fmt.Int(ADR(A[1])-ADR(A[0])) & "\n");
  Msg(Fmt.Int(ADR(A[2])-ADR(A[1])) & "\n");
  Msg(Fmt.Int(ADR(A[0,1])-ADR(A[0,0])) & "\n");
  Msg(Fmt.Int(ADR(A[0,2])-ADR(A[0,1])) & "\n");

  ML.GEES ('N', 'N', NIL, 3, ADR(Am[0]), 3, sdim, eigRe^, eigIm^,
           vs, 1, work^, worksize, bwork, success);

  Msg(VF.Fmt(Am) & "\n");
  Msg(MF.Fmt(A) & "\n");

  FOR j:=0 TO LAST(eigRe^) DO
(*
    Msg(Fmt.FN("%s: %s + i %s\n", ARRAY OF TEXT
         {Fmt.Int(j), Fmt.LongReal(eigRe[j]), Fmt.LongReal(eigIm[j])}));
*)
    Msg(Fmt.FN("%s: %s + i %s\n", ARRAY OF TEXT
         {Fmt.Int(j), Fmt.LongReal(eigRe[j]), Fmt.LongReal(eigIm[j])}));
  END;

  RETURN result;
END TestLinAlg;
(*-------------------------*)
PROCEDURE TestLapack():BOOLEAN=
<*UNUSED*> CONST ftn = Module & "TestLapack";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestBasic();
  NewLine(); EVAL TestLinAlg();
  RETURN result;
END TestLapack;
(*=======================*)
BEGIN
END TestLapack.
