MODULE TestTransition;

IMPORT LongRealBasic               AS R,
       LongRealTrans               AS RT,
       LongRealSignal              AS S,
       LongRealVectorFast          AS V,
       LongRealVectorBasic         AS VB,
       LongRealComplexVectorTrans  AS CVT,
       LongRealMatrixFast          AS M,
       LongRealCharPolynomial      AS CharPoly,
       LongRealComplexRootBasic    AS CRt,
       LongRealRootApproximation   AS RootAppr,
       LongRealEigenSystem         AS Eigen,

       LongRealDaubechiesWavelet   AS Daub,
       LongRealRefinableFunc       AS Refn,

       LongRealFmtLex              AS RF,
       LongRealSignalFmtLex        AS SF,
       LongRealVectorFmtLex        AS VF,
       LongRealComplexVectorFmtLex AS CVF,
       LongRealMatrixFmtLex        AS MF,
       PLPlot AS PL,
       IO, Fmt, Thread, Wr, FloatMode,
       NADefinitions;

PROCEDURE Test()=
  VAR
    mask0 := NEW(S.T).fromArray(ARRAY OF R.T{-2.0D0,0.0D0,1.0D0});
    ev    := Eigen.EigenValuesGen(
               Refn.TransitionMatrix(
                 mask0.adjoint().convolve(mask0)
               )
             );
    x := NEW(V.T,NUMBER(ev.eigenvalues^));
    y := NEW(V.T,NUMBER(ev.eigenvalues^));
  <*FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap, NADefinitions.Error*>
  BEGIN
    IO.Put(CVF.Fmt(ev.eigenvalues));
    FOR i:=0 TO LAST(ev.eigenvalues^) DO
      x[i]:=ev.eigenvalues[i].re;
      y[i]:=ev.eigenvalues[i].im;
    END;
    PL.Init();
    (*PL.SetColor0(1);*)
    PL.SetEnvironment(-10.0D0,10.0D0,-10.0D0,10.0D0);
    PL.PlotPoints(x^,y^,2);
    PL.Exit();
  END Test;

BEGIN
END TestTransition.
