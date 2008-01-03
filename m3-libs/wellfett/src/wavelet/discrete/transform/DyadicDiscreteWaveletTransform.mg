GENERIC MODULE DyadicDiscreteWaveletTransform(R, S, SV, FB, DWT);

IMPORT Arithmetic;
IMPORT Integer32IntegerPower AS IIntPow;


PROCEDURE FromSignal (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ):
  T =
  VAR z := NEW(SV.T, numLevels);
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      WITH lev = DWT.SingleFromSignal(x, y, 2) DO
        z[i] := lev[1];
        x := lev[0];
      END;
    END;
    RETURN T{low := x, high := z};
  END FromSignal;

PROCEDURE ToSignal (READONLY x: T; READONLY y: FB.TBody; ): S.T =
  VAR z := x.low;
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      z := DWT.SingleToSignal(ARRAY OF S.T{z, x.high[i]}, y, 2);
    END;
    RETURN z;
  END ToSignal;

PROCEDURE ShiftInvariantFromSignal
  (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ): T =
  VAR
    z                      := NEW(SV.T, numLevels);
    scaling: S.ScalingType := 1;
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      z[i] := x.upConvolve(y[1], scaling);
      x := x.upConvolve(y[0], scaling);
      scaling := scaling * 2;
    END;
    RETURN T{low := x, high := z};
  END ShiftInvariantFromSignal;

PROCEDURE ShiftInvariantToSignal (READONLY x: T; READONLY y: FB.TBody; ):
  S.T =
  <* FATAL Arithmetic.Error *>   (* Power can't fail for integers *)
  VAR
    z                      := x.low;
    scaling: S.ScalingType := IIntPow.MulPower(1, 2, NUMBER(x.high^));
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      scaling := scaling DIV 2;
      z := z.upConvolve(y[0], scaling).superpose(
             x.high[i].upConvolve(y[1], scaling)).scale(R.Half);
    END;
    RETURN z;
  END ShiftInvariantToSignal;


PROCEDURE HighBandOversampledFromSignal
  (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ): T =
  VAR z := NEW(SV.T, numLevels);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := x.convolve(y[1]);
      x := x.convolveDown(y[0], 2);
    END;
    RETURN T{low := x, high := z};
  END HighBandOversampledFromSignal;

PROCEDURE HighBandOversampledToSignal
  (READONLY x: T; READONLY y: FB.TBody; ): S.T =
  VAR z := x.low;
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      z := y[0].upConvolve(z, 2).superpose(y[1].convolve(x.high[i]));
    END;
    RETURN z;
  END HighBandOversampledToSignal;


PROCEDURE HighBandConvolveDown (x: T; y: S.T; ): T =
  VAR z := NEW(SV.T, NUMBER(x.high^));
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := y.convolveDown(x.high[i], 2);
    END;
    RETURN T{low := x.low, high := z};
  END HighBandConvolveDown;

PROCEDURE HighBandUpConvolve (x: T; y: S.T; ): T =
  VAR z := NEW(SV.T, NUMBER(x.high^));
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := y.upConvolve(x.high[i], 2);
    END;
    RETURN T{low := x.low, high := z};
  END HighBandUpConvolve;


BEGIN
END DyadicDiscreteWaveletTransform.
