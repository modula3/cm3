MODULE TestRefinableFunction;

IMPORT LongRealVectorTrans  AS VT,
       LongRealMatrix       AS M,
       LongRealSignalFmtLex AS SF;

IMPORT LongRealRefinableFunction AS Refn,
       LongRealRefinableSmooth   AS RefnSm;

IMPORT UnitTest, UnitTestList, UnitTestNumeric, UnitTestSignal;

IMPORT Wr, Thread, Random;


PROCEDURE CheckSmoothnessEstimate (SELF: UnitTestNumeric.T; ) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    rnd := NEW(Random.Default).init();
  BEGIN
    FOR n := 1 TO 20 DO
      WITH x = UnitTestSignal.RandomSignal(rnd, rnd.integer(-10, 10), n) DO
        IF NOT SELF.scalarMatch(RefnSm.Frobenius(x),
                                M.Trace(M.MulMMA(Refn.RadicBandMatrix(
                                                   x.autocorrelate()))),
                                5.0D-13 * VT.Norm1(x.getData())) THEN
          SELF.message("signal: " & SF.Fmt(x) & "\n");
        END;
      END;
    END;
  END CheckSmoothnessEstimate;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN NEW(UnitTestList.T).init(
             "refinable functions",
             ARRAY OF
               UnitTest.T{NEW(UnitTestNumeric.T,
                              test := CheckSmoothnessEstimate).init(
                            "Optimisation of Frobenius norm")});
  END Test;

BEGIN
END TestRefinableFunction.
