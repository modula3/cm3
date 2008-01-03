MODULE UnitTestSignal;

IMPORT LongRealBasic        AS R,
       LongRealFmtLex       AS RF,
       LongRealVector       AS V,
       LongRealVectorTrans  AS VT,
       LongRealSignal       AS S,
       LongRealSignalFmtLex AS SF;

IMPORT Wr, Thread, Random;


REVEAL T = Public BRANDED OBJECT OVERRIDES signalMatch := SignalMatch; END;


PROCEDURE SignalMatch (SELF: T; x, y: S.T; tol: R.T; ): BOOLEAN =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    error := VT.Norm1(x.negate().superpose(y).getData());
  BEGIN
    IF error > tol THEN
      SELF.error(
        "Signals don't match, error " & RF.Fmt(error) & ", tolerance "
          & RF.Fmt(tol) & "\n" & SF.Fmt(x) & "\n" & SF.Fmt(y) & "\n");
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END SignalMatch;


PROCEDURE RandomSignal (rnd: Random.T; first: INTEGER; number: CARDINAL; ):
  S.T =
  VAR
    v := V.New(number);
    x := NEW(S.T).fromVector(v, first);
  BEGIN
    FOR i := FIRST(v^) TO LAST(v^) DO v[i] := rnd.longreal(); END;
    RETURN x;
  END RandomSignal;


BEGIN
END UnitTestSignal.
