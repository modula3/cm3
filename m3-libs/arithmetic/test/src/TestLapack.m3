UNSAFE MODULE TestLapack EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)

IMPORT Fmt,Wr,Thread;
IMPORT LongRealBasic              AS R,
       LongRealVectorBasic        AS V,
       (*LongRealComplexVectorBasic AS CV,*)
       (*LongRealMatrixFast         AS M,*)
       LongRealEigenSystem        AS ES,
       LongRealMatrixLapack       AS ML,
       LongRealCharPolynomial     AS MCP,
       LongRealComplexFmtLex      AS CF,
       (*LongRealVectorFmtLex       AS VF,*)
       LongRealMatrixFmtLex       AS MF;
IMPORT NADefinitions;

<*FATAL NADefinitions.Error, Thread.Alerted, Wr.Failure*>
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
         {Fmt.Char(id[j]), Fmt.LongReal(ML.GetMachineParameter(id[j]))}));
  END;

  RETURN result;
END TestBasic;
(*----------------------*)
PROCEDURE TestLinAlg():BOOLEAN=
CONST
  ftn = Module & "TestLinAlg";

VAR
  result:=TRUE;
  (*A:=MCP.CompanionMatrix(V.FromArray(ARRAY OF R.T{1.0D0,3.0D0,3.0D0,1.0D0}));*)
  A:=MCP.CompanionMatrix(V.FromArray(ARRAY OF R.T{-1.0D0,0.0D0,0.0D0,1.0D0}));
  (*
  A:=NEW(REF ARRAY OF ARRAY OF LONGREAL,3,3);
  *)
  ev:ES.EV;
BEGIN
  Debug(1,ftn,"begin\n");

  Msg(MF.Fmt(A) & "\n");

  ev := ES.EigenValuesGen (A, flags:= ES.EVGenFlagSet{ES.EVGenFlag.schurVectors});

  Msg(MF.Fmt(ev.upperTri) & "\n");
  Msg(MF.Fmt(ev.schur) & "\n");

  FOR j:=0 TO LAST(ev.eigenvalues^) DO
    Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT
         {Fmt.Int(j), CF.Fmt(ev.eigenvalues[j])}));
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
