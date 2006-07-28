GENERIC MODULE RefinableFunction(M, S);

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


PROCEDURE Refine (start, mask: S.T;
                  numLevels  : CARDINAL;
                  shift      : CARDINAL   := 2): S.T =
  BEGIN
    WHILE numLevels > 0 DO
      start := mask.upConvolve(start, shift);
      DEC(numLevels);
    END;
    RETURN start;
  END Refine;

BEGIN
END RefinableFunction.
