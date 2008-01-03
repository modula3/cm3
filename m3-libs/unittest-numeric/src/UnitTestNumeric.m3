MODULE UnitTestNumeric;

IMPORT LongRealBasic       AS R,
       LongRealVector      AS V,
       LongRealVectorTrans AS VT,
       LongRealMatrix      AS M,
       LongRealMatrixTrans AS MT;

IMPORT LongRealFmtLex       AS RF,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixFmtLex AS MF;

IMPORT Wr, Thread;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        scalarMatch := ScalarMatch;
        vectorMatch := VectorMatch;
        matrixMatch := MatrixMatch;
      END;



PROCEDURE ScalarMatch (SELF: T; x, y: R.T; tol: R.T; ): BOOLEAN =
  VAR error := ABS(x - y);
  BEGIN
    IF error > tol THEN
      SELF.error(
        "Scalars don't match, error " & RF.Fmt(error) & ", tolerance "
          & RF.Fmt(tol) & "\n" & RF.Fmt(x) & " " & RF.Fmt(y) & "\n");
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END ScalarMatch;

PROCEDURE VectorMatch (SELF: T; x, y: V.T; tol: R.T; ): BOOLEAN =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    error := VT.Norm1(V.Sub(x, y));
  BEGIN
    IF error > tol THEN
      SELF.error(
        "Vectors don't match, error " & RF.Fmt(error) & ", tolerance "
          & RF.Fmt(tol) & "\n" & VF.Fmt(x) & VF.Fmt(y));
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END VectorMatch;

PROCEDURE MatrixMatch (SELF: T; x, y: M.T; tol: R.T; ): BOOLEAN =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    error := MT.Norm1(M.Sub(x, y));
  BEGIN
    IF error > tol THEN
      SELF.error(
        "Matrix don't match, error " & RF.Fmt(error) & ", tolerance "
          & RF.Fmt(tol) & "\n" & MF.Fmt(x) & MF.Fmt(y));
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END MatrixMatch;

BEGIN
END UnitTestNumeric.
