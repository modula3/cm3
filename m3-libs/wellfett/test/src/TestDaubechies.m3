MODULE TestDaubechies;

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
       IO, Fmt, Thread, Wr, FloatMode,
       NADefinitions;

PROCEDURE Test()=

  VAR
    x     : S.T;
    v     : V.T;
    trans : M.T;
    specrad : R.T;
    eig   : REF CRt.RootArray;
  <*FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap, NADefinitions.Error*>
  BEGIN
    FOR n:=0 TO 10 DO
      x := Daub.FilterAbsSqr(n);
      IO.Put(Fmt.FN("%s: %s\n",
        ARRAY OF TEXT{Fmt.Int(n),x.scale(R.Scalb(1.0D0,4*n-2)).fmt()}));
    END;

    x := Daub.FilterPureAbsSqr(3);
    trans := Refn.TransitionMatrix(x);
    IO.Put(Fmt.FN("transition matrix:\n%s",
      ARRAY OF TEXT{MF.Fmt(trans)}));
    eig := RootAppr.RealNewtonMaehli(
             CharPoly.CharPolynomial(trans)
           );
    IO.Put(Fmt.FN("eigenvalues: %s\n", ARRAY OF TEXT{CVF.Fmt(eig)}));
    IO.Put(Fmt.FN("largest eigenvalue: %s\neigenvector: %s\n",
      ARRAY OF TEXT{
        RF.Fmt(Eigen.PowerMethod(trans,v)),
        VF.Fmt(v)}));

    FOR n:=1 TO 45 DO
      x := Daub.FilterPureAbsSqr(n);
      trans := Refn.TransitionMatrix(x);
      specrad := Eigen.SquareMethod(trans,v,tol:=1.0D-6);
      IO.Put(Fmt.FN("%s: spectral radius %s\n",
        ARRAY OF TEXT{Fmt.Int(n),RF.Fmt(specrad)}));
    END;
  END Test;

BEGIN
END TestDaubechies.
