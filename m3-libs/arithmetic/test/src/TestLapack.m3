UNSAFE MODULE TestLapack EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for XYZ module.

1/1/96    <name>   Initial version

*)

IMPORT Fmt;
IMPORT LongRealBasic          AS R,
       LongRealVectorBasic    AS V,
       LongRealVectorFmtLex   AS VF,
       LongRealMatrixFast     AS M,
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
  Atmp:=M.Transpose(A);
  eigRe:=V.New(3);
  eigIm:=V.New(3);
  sdim:INTEGER;
  worksize:=MAX(1,3*size);
  work:=V.New(worksize);
  vs:LONGREAL;
  bwork:=NEW(REF ARRAY OF BOOLEAN,size);
  success:INTEGER;
BEGIN
  Debug(1,ftn,"begin\n");

  Msg(MF.Fmt(Atmp) & "\n");

  ML.GEES ('N', 'N', NIL, 3, Atmp[0,0], 3, sdim, eigRe[0], eigIm[0],
           vs, 1, work[0], worksize, bwork[0], success);

  Msg(MF.Fmt(Atmp) & "\n");
  A:=M.Transpose(Atmp);
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
