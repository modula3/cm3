GENERIC MODULE RefinableFunc(R, CVT, M, Eigen, S);

IMPORT NADefinitions AS NA;

PROCEDURE RadicBandMatrix (mask: S.T; shift: CARDINAL := 2): M.T =
  VAR
    (*the size of the matrix is the maximum possible with still avoiding of
       zeros on the diagonal from outside the support of the mask*)
    n := mask.getNumber() DIV (shift - 1);
    z := M.NewZero(n, n);
  BEGIN
    FOR j := 0 TO n - 1 DO
      mask.clipToArray(mask.getLast() - j * shift, z[j]);
    END;
    RETURN z;
  END RadicBandMatrix;

PROCEDURE TransitionMatrix (mask: S.T; shift: CARDINAL := 2): M.T =
  BEGIN
    RETURN RadicBandMatrix(mask.autocorrelate(), shift);
  END TransitionMatrix;

PROCEDURE TransitionEV (mask: S.T): Eigen.EV RAISES {NA.Error} =
  BEGIN
    RETURN Eigen.EigenValues(TransitionMatrix(mask));
  END TransitionEV;

PROCEDURE TransitionSpecRad (mask: S.T): R.T RAISES {NA.Error} =
  BEGIN
    RETURN CVT.NormInf(TransitionEV(mask).eigenvalues);
  END TransitionSpecRad;

PROCEDURE Refine (start, mask: S.T; levels: CARDINAL; shift: CARDINAL := 2):
  S.T =
  BEGIN
    WHILE levels > 0 DO
      start := start.upsample(shift).convolve(mask);
      DEC(levels);
    END;
    RETURN start;
  END Refine;

BEGIN
END RefinableFunc.
