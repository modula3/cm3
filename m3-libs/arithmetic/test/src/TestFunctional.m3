MODULE TestFunctional EXPORTS Test;
(*Copyright (c) 1996, m3na project*)

IMPORT LongRealBasic       AS R,
       LongRealTrans       AS RT,
       LongRealVector      AS V,
       LongRealVectorTrans AS VT,
       LongRealMatrix      AS M,
       LongRealMatrixTrans AS MT,

       LongRealFmtLex       AS RF,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixFmtLex AS MF;

IMPORT LongRealFunctional AS Fn, LongRealFunctionalDeriv2 AS FnD;

IMPORT IO, Fmt;

(*=======================*)
CONST Module = "TestFunctional.";
(*----------------------*)
PROCEDURE TestDeriv (): BOOLEAN =
  <*FATAL ANY*>

  PROCEDURE SquareForm (x: V.T): R.T =
    BEGIN
      RETURN V.Inner(x, M.MulV(mat, x));
    END SquareForm;

  CONST ftn = Module & "TestDeriv";
  VAR
    result            := TRUE;
    mat               := NEW(M.T, 5, 5);
    vec               := NEW(V.T, 5);
    dvec              := NEW(V.T, 5);
    diff, der : FnD.T;
    err1, err2: R.T;

  BEGIN
    Debug(1, ftn, "begin\n");
    FOR i := FIRST(mat^) TO LAST(mat^) DO
      vec[i] := RT.Cos(FLOAT(i, R.T));
      (*choose this around a third of the number precision, don't make it
         to small otherwise heavy cancelations will occur*)
      dvec[i] := 1.0D-5;
      FOR j := FIRST(mat[0]) TO LAST(mat[0]) DO
        mat[i, j] := RT.Sin(FLOAT(i + j + i * j, R.T));
      END;
    END;

    diff := Fn.EvalCentralDiff2(SquareForm, vec, dvec);
    der.first := V.Scale(M.MulV(mat, vec), R.Two);
    der.second := M.Scale(mat, R.Two);
    err1 := VT.NormInf(V.Sub(diff.first, der.first));
    err2 := MT.NormInf(M.Sub(diff.second, der.second));
    IO.Put(
      Fmt.FN("first:\n   %s ~ %s (dif %s)\nsecond:\n   %s ~ %s (dif %s)\n",
             ARRAY OF
               TEXT{VF.Fmt(der.first), VF.Fmt(diff.first), RF.Fmt(err1),
                    MF.Fmt(der.second), MF.Fmt(diff.second), RF.Fmt(err2)}));
    <*ASSERT ABS(err1)<1.0D-7*>
    <*ASSERT ABS(err2)<1.0D-5*>
    RETURN result;
  END TestDeriv;
(*-------------------------*)
PROCEDURE TestFunctional (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestFunctional";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestDeriv();
    RETURN result;
  END TestFunctional;
(*=======================*)
BEGIN
END TestFunctional.
