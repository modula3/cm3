MODULE TestSignal;

IMPORT LongRealSignalFmtLex AS SF, Arithmetic AS Arith;

IMPORT UnitTest, UnitTestList, UnitTestSignal;

IMPORT Fmt, Wr, Thread, Random;


PROCEDURE CheckDeconvolveMod (SELF: UnitTestSignal.T; ) =
  <* FATAL Thread.Alerted, Wr.Failure, Arith.Error *>
  VAR
    rnd := NEW(Random.Default).init();
  BEGIN
    FOR n := 1 TO 10 DO
      WITH dn = rnd.integer(0, 15),
           x = UnitTestSignal.RandomSignal(
                 rnd, rnd.integer(-10, 10), n + dn),
           y = UnitTestSignal.RandomSignal(rnd, rnd.integer(-10, 10), n) DO
        (* assert that the boundary values are big enough, otherwise we get
           bad numerical results *)
        WITH yData = y.getData()^ DO
          yData[0] := rnd.longreal(0.9D0, 2.0D0);
          yData[LAST(yData)] := rnd.longreal(0.9D0, 2.0D0);
        END;
        WITH qrs = x.deconvolveModAll(y) DO
          FOR j := 0 TO dn DO
            WITH qr = x.deconvolveMod(y, j) DO
              IF NOT SELF.signalMatch(
                       x, y.convolve(qr.quot).superpose(qr.rem), 1.0D-12) THEN
                SELF.message(
                  "no reconstruction on " & SF.Fmt(x) & " div " & SF.Fmt(y)
                    & "\nquotient " & SF.Fmt(qr.quot) & ", remainder "
                    & SF.Fmt(qr.rem) & "\n");
              END;
              IF NOT SELF.signalMatch(qr.quot, qrs[j].quot, 1.0D-13) THEN
                SELF.message(
                  Fmt.Int(j)
                    & "th quotient of deconvolveMod and deconvolveModAll differ on "
                    & SF.Fmt(x) & " div " & SF.Fmt(y) & "\n");
              END;
              IF NOT SELF.signalMatch(qr.rem, qrs[j].rem, 1.0D-13) THEN
                SELF.message(
                  Fmt.Int(j)
                    & "th remainder of deconvolveMod and deconvolveModAll differ on "
                    & SF.Fmt(x) & " div " & SF.Fmt(y) & "\n");
              END;
            END;
          END;
        END;
      END;
    END;
  END CheckDeconvolveMod;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN NEW(UnitTestList.T).init(
             "signal", ARRAY OF
                         UnitTest.T{NEW(UnitTestSignal.T,
                                        test := CheckDeconvolveMod).init(
                                      "Deconvolution with remainder")});
  END Test;

BEGIN
END TestSignal.
