GENERIC MODULE DiscreteWaveletTransform(R, S, VS, VSR, MS);

IMPORT NADefinitions AS NA;
IMPORT Integer32IntegerPower AS IIntPow;

PROCEDURE FilterBankToPolyphase (READONLY x      : VS.TBody;
                                          scaling: ScalingType; ): MS.T =
  VAR z := NEW(MS.T, NUMBER(x), scaling);
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := x[i].sliceRev(scaling)^; END;
    RETURN z;
  END FilterBankToPolyphase;

PROCEDURE PolyphaseToFilterBank (READONLY x: MS.TBody; ): VS.T =
  VAR z := NEW(VS.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      z[i] := NEW(S.T).interleaveRev(x[i]);
    END;
    RETURN z;
  END PolyphaseToFilterBank;

PROCEDURE FilterBankAnalysisSingle (         x      : S.T;
                                    READONLY y      : VS.TBody;
                                             scaling: ScalingType; ):
  VS.T =
  VAR z := NEW(VS.T, NUMBER(y));
  BEGIN
    FOR i := FIRST(y) TO LAST(y) DO
      z[i] := x.convolveDown(y[i], scaling);
    END;
    RETURN z;
  END FilterBankAnalysisSingle;

PROCEDURE FilterBankSynthesisSingle (READONLY x, y   : VS.TBody;
                                              scaling: ScalingType; ): S.T
  RAISES {NA.Error} =
  VAR z := S.Zero;
  BEGIN
    IF NUMBER(x) # NUMBER(y) THEN RAISE NA.Error(NA.Err.bad_size); END;
    FOR i := FIRST(x) TO LAST(x) DO
      z := z.superpose(y[i].upConvolve(x[i], scaling));
    END;
    RETURN z;
  END FilterBankSynthesisSingle;

<*INLINE*>
PROCEDURE FilterBankAnalysisTISingle (x: S.T; READONLY filter: VS.TBody; ):
  VS.T =
  BEGIN
    (*exchange argument order*)
    RETURN VSR.Scale(filter, x);
  END FilterBankAnalysisTISingle;

(*
PROCEDURE FilterBankAnalysis (         x        : S.T;
                               READONLY y        : VS.TBody;
                                        numLevels: CARDINAL; ):
  WaveletCoeffs =
  VAR
    z := WaveletCoeffs{
           low := NIL, high := NEW(MS.T, numLevels, NUMBER(y) - 1)};
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      VAR lev := FilterBankAnalysisSingle(x, y);
      BEGIN
        z.high[i] := SUBARRAY(lev^, 1, NUMBER(y) - 1);
      END;
    END;
  END FilterBankAnalysis;
*)

PROCEDURE DyadicFilterBankAnalysis (         x: S.T;
                                    READONLY y: ARRAY [0 .. 1] OF S.T;
                                    numLevels: CARDINAL; ):
  DyadicWaveletCoeffs =
  VAR z := NEW(VS.T, numLevels);
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      VAR lev := FilterBankAnalysisSingle(x, y, 2);
      BEGIN
        z[i] := lev[1];
        x := lev[0];
      END;
    END;
    RETURN DyadicWaveletCoeffs{low := x, high := z};
  END DyadicFilterBankAnalysis;

PROCEDURE DyadicFilterBankSynthesis (READONLY x: DyadicWaveletCoeffs;
                                     READONLY y: ARRAY [0 .. 1] OF S.T; ):
  S.T =
  VAR z := x.low;
  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      z := FilterBankSynthesisSingle(ARRAY OF S.T{z, x.high[i]}, y, 2);
    END;
    RETURN z;
  END DyadicFilterBankSynthesis;

PROCEDURE DyadicFilterBankAnalysisTI (         x: S.T;
                                      READONLY y: ARRAY [0 .. 1] OF S.T;
                                      numLevels: CARDINAL; ):
  DyadicWaveletCoeffs =
  VAR
    z                    := NEW(VS.T, numLevels);
    scaling: ScalingType := 1;
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      z[i] := x.upConvolve(y[1], scaling);
      x := x.upConvolve(y[0], scaling);
      scaling := scaling * 2;
    END;
    RETURN DyadicWaveletCoeffs{low := x, high := z};
  END DyadicFilterBankAnalysisTI;

PROCEDURE DyadicFilterBankSynthesisTI (READONLY x: DyadicWaveletCoeffs;
                                       READONLY y: ARRAY [0 .. 1] OF S.T; ):
  S.T =
  VAR
    z                    := x.low;
    scaling: ScalingType := IIntPow.MulPower(1, 2, NUMBER(x.high^));
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      scaling := scaling DIV 2;
      z := z.upConvolve(y[0], scaling).superpose(
             x.high[i].upConvolve(y[1], scaling)).scale(R.Half);
    END;
    RETURN z;
  END DyadicFilterBankSynthesisTI;

BEGIN
END DiscreteWaveletTransform.
