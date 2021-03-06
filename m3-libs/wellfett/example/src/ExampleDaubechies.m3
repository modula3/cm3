MODULE ExampleDaubechies;

IMPORT LongRealBasic  AS R,
       LongRealTrans  AS RT,
       LongRealSignal AS S,
       LongRealVector AS V,
       (*
              LongRealComplexVectorTrans  AS CVT,
       *)
       LongRealMatrix              AS M,
       LongRealCharPolynomial      AS CharPoly,
       LongRealComplexRoot         AS CRt,
       LongRealRootApproximation   AS RootAppr,
       LongRealEigenSystem         AS Eigen,
       LongRealDaubechiesWavelet   AS Daub,
       LongRealRefinableFunction       AS Refn,
       LongRealFmtLex              AS RF,
       LongRealSignalFmtLex        AS SF,
       LongRealVectorFmtLex        AS VF,
       LongRealComplexVectorFmtLex AS CVF,
       LongRealMatrixFmtLex        AS MF,
       IO,
       Fmt,
       Thread,
       Wr,
       FloatMode,
       Arithmetic;

PROCEDURE Run () =

  VAR
    x                  : S.T;
    yv                 : V.T;
    trans              : M.T;
    sqr0, sqr1, specrad: R.T;
    eig                : REF CRt.RootArray;
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap,
    Arithmetic.Error *>
  BEGIN
    FOR n := 0 TO 10 DO
      x := Daub.FilterAbsSqr(n);
      IO.Put(
        Fmt.FN(
          "%s: %s\n",
          ARRAY OF
            TEXT{Fmt.Int(n), x.scale(R.Scalb(1.0D0, 4 * n - 2)).fmt()}));
    END;

    x := Daub.FilterPureAbsSqr(3);
    trans := Refn.RadicBandMatrix(x);
    IO.Put(Fmt.FN("transition matrix:\n%s", ARRAY OF TEXT{MF.Fmt(trans)}));
    eig := RootAppr.RealNewtonMaehli(CharPoly.CharPolynomial(trans));
    IO.Put(Fmt.FN("eigenvalues: %s\n", ARRAY OF TEXT{CVF.Fmt(eig)}));
    VAR ep := Eigen.PowerMethod(trans);
    BEGIN
      IO.Put(Fmt.FN("largest eigenvalue: %s\neigenvector: %s\n",
                    ARRAY OF TEXT{RF.Fmt(ep.value), VF.Fmt(ep.vector)}));
    END;

    FOR n := 1 TO 45 DO
      x := Daub.FilterPureAbsSqr(n);
      trans := Refn.RadicBandMatrix(x);
      yv := x.wrapCyclic(3);
      sqr0 := yv[0] + yv[1] + yv[2];
      sqr1 := M.Trace(trans);
      IO.Put(Fmt.FN("%s: sum of the eigenvalues: %s ~ %s\n",
                    ARRAY OF TEXT{Fmt.Int(n), RF.Fmt(sqr0), RF.Fmt(sqr1)}));
      <* ASSERT ABS(sqr0 - sqr1) < 1.0D0 * sqr0 *>
      sqr0 := yv[0] * yv[0] + yv[1] * yv[1] + yv[2] * yv[2];
      sqr1 := M.Trace(M.Mul(trans, trans));
      (*
            IO.Put(Fmt.FN("%s: sum of the eigenvalue squares: %s ~ %s\n",
              ARRAY OF TEXT{Fmt.Int(n),RF.Fmt(sqr0),RF.Fmt(sqr1)}));
            <*ASSERT ABS(sqr0-sqr1)<1.0D-10*sqr0*>
      *)
      specrad := Eigen.SquareMethod(trans, tol := 1.0D-5).value;
      IO.Put(
        Fmt.FN("%s: spectral radius %s < %s <(?) %s\n",
               ARRAY OF
                 TEXT{Fmt.Int(n),
                      RF.Fmt(RT.SqRt(sqr0 / FLOAT(x.getNumber(), R.T))),
                      RF.Fmt(specrad), RF.Fmt(RT.SqRt(sqr0))}));
    END;
  END Run;

BEGIN
END ExampleDaubechies.
