MODULE TestVector EXPORTS Test;
(*Arithmetic for Modula-3, see doc for details Abstract: Tests for Vector module.

   1/1/96 <name> Initial version

   *)
(*FROM NADefinitions IMPORT Error,Err;*)
IMPORT                           (*LongRealBasic AS R,*)
  LongRealFmtLex       AS RF,
  LongRealVector       AS V,
  LongRealVectorTrans  AS VT,
  LongRealVectorFmtLex AS VF;

(*=======================*)
CONST Module = "TestVector.";

(*----------------------*)
<*FATAL ANY*>
PROCEDURE TestVectorBasic (): BOOLEAN =
  CONST
    ftn = Module & "TestVectorBasic";
    n   = 4;
  VAR
    result      := TRUE;
    v1          := V.New(n);
    v2    : V.T;
  BEGIN
    Debug(1, ftn, "begin\n");
    (*V.Zero(v1); Msg("zero =" & VF.Fmt(v1) & "\n");*)

    v1[0] := 0.0D0;
    v1[1] := 1.0D0;
    v1[2] := 2.0D0;
    v1[3] := 3.0D0;
    Msg("v1       =" & VF.Fmt(v1) & "\n");
    v2 := V.Copy(v1);
    Msg("copy(v1) =" & VF.Fmt(v2) & "\n");
    Msg("|v1|     =" & RF.Fmt(VT.Norm2(v1)) & "\n");

    v2 := V.Scale(v2, 3.0D0);
    Msg("v1*3.0   =" & VF.Fmt(v2) & "\n");

    Msg("v2       =" & VF.Fmt(v2) & "\n");
    Msg("v1+v2    =" & VF.Fmt(V.Add(v1, v2)) & "\n");
    Msg("v1-v2    =" & VF.Fmt(V.Sub(v1, v2)) & "\n");
    Msg("v1 dot v2=" & RF.Fmt(V.Inner(v2, v1)) & "\n");
    (*
      TRY
        Msg("v1 x v2  =");
        Msg(VF.Fmt(V.cross(v2,v1)) & "\n");
      EXCEPT
      | Error(err) => Msg("not implemented\n");
      END;
    *)

    RETURN result;
  END TestVectorBasic;
(*-------------------------*)
PROCEDURE TestVector (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestVector";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestVectorBasic();
    RETURN result;
  END TestVector;
(*=======================*)
BEGIN
END TestVector.
