MODULE TestPerfectReconstruction;

IMPORT LongRealBasic              AS R,
       LongRealTrans              AS RT,
       LongRealVector             AS V,
       LongRealSignal             AS S,
       LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealBSplineWavelet                 AS BSpline,
       LongRealWaveletMatchBasis              AS WMBasis,
       LongRealWaveletMatchSmooth             AS WaveletMatchSmooth,
       LongRealDyadicFilterBank               AS FilterBank,
       LongRealDyadicDiscreteWaveletTransform AS DWT;

IMPORT UnitTest, UnitTestList, UnitTestSignal;

IMPORT Fmt, Random;
IMPORT Arithmetic, Range;


PROCEDURE CheckPerfectReconstruction (SELF: UnitTestSignal.T; ) =
  <* FATAL Arithmetic.Error *>   (* What to do if the matching algorithm
                                    does not converge? *)

  PROCEDURE CheckBank
    (name: TEXT; READONLY primal, dual: FilterBank.TBody; tol: R.T; ) =
    BEGIN
      WITH prod0      = primal[0].convolve(dual[0]),
           prod1      = primal[1].convolve(dual[1]),
           almostOne  = prod0.superpose(prod1),
           prod0a     = primal[0].alternate().convolve(dual[0]),
           prod1a     = primal[1].alternate().convolve(dual[1]),
           almostZero = prod0a.superpose(prod1a)                 DO

        IF NOT SELF.signalMatch(almostOne, S.One, tol)
             OR NOT SELF.signalMatch(almostZero, S.Zero, tol) THEN
          SELF.message(name & "\n");
        END;
      END;
    END CheckBank;

  BEGIN
    SELF.message("Check perfect reconstruction of B-Spline wavelets.\n");
    FOR i := 0 TO 6 DO
      FOR j := i MOD 2 TO 8 BY 2 DO
        IF NOT (i = 0 AND j = 0) THEN
          WITH dual = FilterBank.TBody{BSpline.GeneratorMask(i),
                                       BSpline.WaveletMask(i, j)},
               primal = FilterBank.DualToPrimal(dual) DO
            (* We compare with R.Zero because there are no rounding errors.
               The coefficients of the CDF wavelets have powers of two as
               denominators. *)
            CheckBank(Fmt.F("CDF-%s,%s", Fmt.Int(i), Fmt.Int(j)), primal,
                      dual, R.Zero);
          END;
        END;
      END;
    END;

    SELF.message("Check perfect reconstruction of matched wavelet.\n");
    WITH rnd = NEW(Random.Default).init(),
         v   = V.New(50)                   DO
      FOR i := FIRST(v^) TO LAST(v^) DO v[i] := rnd.longreal(); END;
      WITH filterBank = WaveletMatchSmooth.BSplineWithPattern(
                          NEW(S.T).fromVector(v, -25), 4, 2, 2,
                          Range.New(-5, 10), 2, R.Zero) DO
        CheckBank("matched wavelet", filterBank[0], filterBank[1], 1.0D-12);
      END;
    END;
  END CheckPerfectReconstruction;

PROCEDURE CheckTransformReconstruction (SELF: UnitTestSignal.T; ) =
  CONST
    numLevels     = 4;
    dualSmooth    = 4;
    dualVanishing = 2;
    primalSmooth  = 6;

  VAR
    rnd      := NEW(Random.Default).init();
    x  : S.T;

  PROCEDURE CheckReconstructed (msg: TEXT; recon: S.T; ) =
    BEGIN
      SELF.message("   " & msg & "\n");
      EVAL SELF.signalMatch(recon, x, 1.0D-12);
    END CheckReconstructed;

  <* FATAL Arithmetic.Error *>   (* SIntPow.MulPower can't fail *)
  VAR
    dual := FilterBank.TBody{
              BSpline.GeneratorMask(dualSmooth).scale(RT.SqRtTwo),
              BSpline.WaveletMask(dualSmooth, primalSmooth).scale(
                RT.SqRtTwo)};
    primal := FilterBank.DualToPrimal(dual);
    vanishing := SIntPow.MulPower(S.One, WMBasis.vanishingAtom,
                                  primalSmooth - dualVanishing);

  BEGIN
    WITH v = V.New(100) DO
      FOR i := FIRST(v^) TO LAST(v^) DO v[i] := rnd.longreal(); END;
      x := NEW(S.T).fromVector(v, -42);
    END;

    CheckReconstructed(
      "DWT forward",
      DWT.ToSignal(DWT.FromSignal(x, primal, numLevels), dual));

    CheckReconstructed(
      "DWT backward",
      DWT.ToSignal(DWT.FromSignal(x, dual, numLevels), primal));

    CheckReconstructed("SWT forward", DWT.ShiftInvariantToSignal(
                                        DWT.ShiftInvariantFromSignal(
                                          x, primal, numLevels), dual));

    CheckReconstructed("SWT backward", DWT.ShiftInvariantToSignal(
                                         DWT.ShiftInvariantFromSignal(
                                           x, dual, numLevels), primal));


    dual[1] := BSpline.WaveletMask(dualSmooth + primalSmooth
                                     - dualVanishing, dualVanishing).scale(
                 RT.SqRtTwo);

    CheckReconstructed(
      "DWT high band oversampled forward",
      DWT.ToSignal(DWT.HighBandConvolveDown(
                     DWT.HighBandOversampledFromSignal(x, dual, numLevels),
                     vanishing), primal));

    CheckReconstructed(
      "DWT high band oversampled backward",
      DWT.HighBandOversampledToSignal(
        DWT.HighBandUpConvolve(
          DWT.FromSignal(x, primal, numLevels), vanishing), dual));

  END CheckTransformReconstruction;

PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN
      NEW(UnitTestList.T).init(
        "perfect reconstruction",
        ARRAY OF
          UnitTest.T{
          NEW(UnitTestSignal.T, test := CheckPerfectReconstruction).init(
            "perfect reconstruction of certain filter banks"),
          NEW(
            UnitTestSignal.T, test := CheckTransformReconstruction).init(
            "perfect reconstruction of transforms")});
  END Test;

BEGIN
END TestPerfectReconstruction.
