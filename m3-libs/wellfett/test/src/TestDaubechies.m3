MODULE TestDaubechies;

IMPORT LongRealBasic             AS R,
       LongRealTrans             AS RT,
       LongRealSignal            AS S,
       LongRealMatrixFast        AS M,
       LongRealCharPolynomial    AS CharPoly,
       LongRealComplexRootBasic  AS CRt,
       LongRealRootApproximation AS RootAppr,
       LongRealDaubechiesWavelet AS Daub,
       LongRealRefinableFunc     AS Refn,
       LongRealSignalFmtLex        AS SF,
       LongRealComplexVectorFmtLex AS CVF,
       LongRealMatrixFmtLex        AS MF,
       IO, Fmt, Thread, Wr, FloatMode,
       NADefinitions;

PROCEDURE Test()=
  VAR
    x   : S.T;
    eig : REF CRt.RootArray;
  <*FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap, NADefinitions.Error*>
  BEGIN
    FOR n:=0 TO 10 DO
      x := Daub.FilterAbsSqr(n);
      IO.Put(Fmt.FN("%s: %s\n",
        ARRAY OF TEXT{Fmt.Int(n),x.scale(R.Scalb(1.0D0,4*n-2)).fmt()}));
    END;
    x := Daub.FilterAbsSqr(3);
    IO.Put(Fmt.FN("transition matrix:\n%s",
      ARRAY OF TEXT{MF.Fmt(Refn.TransitionMatrix(x))}));
    eig := RootAppr.RealNewtonMaehli(
             CharPoly.CharPolynomial(Refn.TransitionMatrix(x))
           );
    IO.Put(Fmt.FN("eigenvalues: %s\n", ARRAY OF TEXT{CVF.Fmt(eig)}));
  END Test;

BEGIN
END TestDaubechies.
