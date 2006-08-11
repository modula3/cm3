GENERIC MODULE WaveletMatchSmooth(R, RIntPow, RT, RF, V, VT, VF, M, LA, MF,
                                  S, SF, FB, Fn, FnD, Refn, WMMatch,
                                  WMBasis, WMGrad, WMGradLift, PL, WPlot);

IMPORT IO, Fmt, Wr, Thread;

IMPORT Integer32IntegerPower AS IIntPow;
IMPORT Range;
IMPORT Arithmetic;


PROCEDURE WithPattern (         target      : S.T;
                                dualBasis   : WMBasis.T;
                                translates  : Range.T;
                                numLevels   : CARDINAL;
                                smoothWeight: R.T;
                                flags       : FlagSet;
                       READONLY options     : Options;   ):
  WMGrad.Parameters RAISES {Arithmetic.Error} =

  <* FATAL Thread.Alerted, Wr.Failure *>
  PROCEDURE CheckDerivatives () =
    CONST
      delta = 1.0D-8;
      cf    = 0.7D0;
    VAR
      (* this array length can only be used if a total of 6 translates is
         considered *)
      y := NEW(S.T).fromArray(
             ARRAY OF R.T{0.2D0, -0.3D0, 0.0D0, -0.1D0, 0.0D0, 0.4D0}, -3);
      der := FnD.Add(WMGradLift.DeriveDist(
                       normalMat, waveletCor, waveletNormSqr, y),
                     FnD.Scale(
                       WMGradLift.DeriveSSE(
                         dualBasis.lpVan, dualBasis.hp, y), smoothWeight));
      derArr := NEW(REF ARRAY OF FnD.T, NUMBER(der.first^));
      extDer := WMGrad.ExtendDervTarget(
                  der, y.getData(), cf, targetVec, targetCor, waveletVec);
      extDerArr := NEW(REF ARRAY OF FnD.T, NUMBER(extDer.first^));

    BEGIN
      FOR j := 0 TO LAST(derArr^) DO
        WITH yp = y.superpose(NEW(S.T).fromArray(
                                ARRAY OF R.T{delta}, j + y.getFirst())) DO
          derArr[j] :=
            FnD.Add(WMGradLift.DeriveDist(
                      normalMat, waveletCor, waveletNormSqr, yp),
                    FnD.Scale(
                      WMGradLift.DeriveSSE(
                        dualBasis.lpVan, dualBasis.hp, yp), smoothWeight));
          extDerArr[j] :=
            WMGrad.ExtendDervTarget(derArr[j], yp.getData(), cf, targetVec,
                                    targetCor, waveletVec);
        END;
      END;
      extDerArr[LAST(extDerArr^)] :=
        WMGrad.ExtendDervTarget(
          der, y.getData(), cf + delta, targetVec, targetCor, waveletVec);

      IO.Put(Fmt.FN("y %s, cf %s\n", ARRAY OF TEXT{SF.Fmt(y), RF.Fmt(cf)}));
      WMGrad.PutDervDif(der, derArr^, delta);
      WMGrad.PutDervDif(extDer, extDerArr^, delta);
    END CheckDerivatives;

  VAR
    generatorVan := Refn.Refine(dualBasis.getLSGeneratorMask(),
                                dualBasis.getRefineMask(), numLevels);
    wavelet := Refn.Refine(dualBasis.getLSWavelet0Mask(),
                           dualBasis.getRefineMask(), numLevels);

    unit   := IIntPow.MulPower(1, 2, numLevels);
    twonit := 2 * unit;
    first := MIN(
               wavelet.getFirst(),
               generatorVan.getFirst() + twonit * Range.First(translates));
    last := MAX(wavelet.getLast(),
                generatorVan.getLast() + twonit * Range.Last(translates));
    size      := last - first + 1;
    waveScale := FLOAT(unit, R.T);
    grid      := R.One / FLOAT(unit, R.T);
    abscissa  := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

    waveletVec     := wavelet.clipToVector(first, size);
    waveletNormSqr := V.Inner(waveletVec, waveletVec);
    targetVec      := target.clipToVector(first, size);
    targetNormSqr  := V.Inner(targetVec, targetVec);
    (* the target vector might have been clipped, thus
       V.Inner(target.getData(),target.getData()) may be different *)

    basis := WMMatch.TranslatesBasis(
               generatorVan, Range.New(first, size), translates, twonit);
    normalMat  := M.MulMMA(basis);
    targetCor  := M.MulV(basis, targetVec);
    waveletCor := M.MulV(basis, waveletVec);

  BEGIN
    IF Flag.Plot IN flags THEN
      PL.StartPage();
      WPlot.Frame(abscissa^, M.Scale(basis, waveScale)^,
                  V.Scale(waveletVec, waveScale)^, targetVec^);
      PL.StopPage();
    END;

    IF Flag.Debug IN flags THEN
      CheckDerivatives();
      IO.Put(
        Fmt.FN("normal matrix %s\n", ARRAY OF TEXT{MF.Fmt(normalMat)}));
    END;

    VAR matching: WMGrad.T;

    <* UNUSED *>
    PROCEDURE ComputeOptCritDeriv (x: V.T): FnD.T =
      VAR
        (* SplitParamVec may return initWavelet0Amp as waveletAmp and this
           won't work if we compute the real derivative instead of a finite
           difference. *)
        mc := matching.splitParamVec(x);
        derDist := WMGradLift.DeriveDist(
                     normalMat, targetCor, targetNormSqr, mc.lift);
        derWavDist := WMGrad.ExtendDervTarget(
                        derDist, mc.lift.getData(), mc.wavelet0Amp,
                        waveletVec, waveletCor, targetVec);
      BEGIN
        RETURN FnD.Add(derWavDist,
                       FnD.Scale(WMGradLift.DeriveWSSE(
                                   dualBasis.getLSGeneratorMask(),
                                   dualBasis.getLSWavelet0Mask(), mc.lift,
                                   mc.wavelet0Amp), smoothWeightFade));
      END ComputeOptCritDeriv;

    PROCEDURE ComputeOptCritDiff (x: V.T): FnD.T
      RAISES {Arithmetic.Error} =

      PROCEDURE EstimateSmoothness (x: V.T): R.T =
        VAR
          hpDual := dualBasis.getLiftedWaveletMaskNoVan(
                      matching.splitParamVec(x));
          lpPrimal := FB.DualHighPassToPrimalLowPass(hpDual);
        (* lpPrimal contains none of the smoothness factor (1,1) of the
           final low pass, because we want to optimize the fractal part of
           the primal generator only. *)
        <* FATAL Arithmetic.Error *>
        BEGIN
          RETURN options.smoothness(lpPrimal);
        END EstimateSmoothness;

      VAR
        mc := matching.splitParamVec(x);
        derDist := WMGradLift.DeriveDist(
                     normalMat, targetCor, targetNormSqr, mc.lift);

        dx  := V.New(NUMBER(x^));
        dxv := VT.Norm1(x) * options.difDist;
        (* dx := V.Scale(x, 1.0D-2); *)
        derWavDist := matching.deriveRegularized(
                        derDist, mc, waveletVec, waveletCor, targetVec);
        derSmooth: FnD.T;

      <* FATAL Thread.Alerted, Wr.Failure *>
      BEGIN
        IF Flag.Verbose IN flags THEN
          IO.Put(
            Fmt.FN("ComputeOptCritDiff for x=%s", ARRAY OF TEXT{VF.Fmt(x)}));
        END;

        FOR i := FIRST(dx^) TO LAST(dx^) DO dx[i] := dxv END;
        derSmooth := Fn.EvalCentralDiff2(EstimateSmoothness, x, dx);

        IF Flag.Verbose IN flags THEN
          IO.Put(Fmt.FN("dist %s, smooth %s, weight %s\n",
                        ARRAY OF
                          TEXT{RF.Fmt(derWavDist.zeroth),
                               RF.Fmt(derSmooth.zeroth),
                               RF.Fmt(smoothWeightFade)}));
          IO.Put(
            Fmt.FN("dist' %ssmooth' %s\n", ARRAY OF
                                             TEXT{VF.Fmt(derWavDist.first),
                                                  VF.Fmt(derSmooth.first)}));
        END;

        RETURN FnD.Add(derWavDist, FnD.Scale(derSmooth, smoothWeightFade));
      END ComputeOptCritDiff;

    PROCEDURE PlotBase () =
      BEGIN
        (* draw resulting wavelet base *)
        IF Flag.Plot IN flags THEN
          PL.StartPage();
          dualBasis.plotBase(matching.splitParamVec(x), numLevels);
          PL.StopPage();
        END;
      END PlotBase;


    VAR
      (*
      x := V.FromVectorArray(
             ARRAY OF V.T{V.New(2 * numTranslates), V.FromScalar(R.One)});
      *)
      (*
      x := V.FromVectorArray(
             ARRAY OF
               V.T{V.ArithSeq(2 * numTranslates, -0.45D0, 0.1D0),
                   V.FromScalar(R.One)});
      *)
      (* use this initialization if you want to compare the results with
         MatchPattern

         x := V.New(2 * numTranslates + 1); *)
      initLift := NEW(S.T).init(translates.first, translates.number);
      initDerDist := WMGradLift.DeriveDist(
                       normalMat, targetCor, targetNormSqr, initLift);
      initDerWavDist := WMGrad.ExtendDervTarget(
                          initDerDist, initLift.getData(), R.Zero,
                          waveletVec, waveletCor, targetVec);
      x := V.Neg(LA.LeastSquares(initDerWavDist.second,
                                 ARRAY OF V.T{initDerWavDist.first})[0].x);

      smoothWeightFade := smoothWeight / RIntPow.MulPower(
                            R.One, options.smoothWeightProgress,
                            options.maxIter);

    BEGIN
      IF options.variableWaveletAmplitude THEN
        matching :=
          NEW(WMGrad.VariableWaveletAmplitude, yFirst := translates.first);
      ELSE
        matching := NEW(WMGrad.FixedWaveletAmplitude,
                        yFirst := translates.first, wavAmp := x[LAST(x^)]);
      END;

      IF Flag.Plot IN flags THEN PL.StartPage(); END;
      PlotBase();

      IF Flag.Debug IN flags THEN
        IO.Put(Fmt.FN("targetCor %s", ARRAY OF TEXT{VF.Fmt(targetCor)}));
      END;

      FOR iter := 1 TO options.maxIter DO
        smoothWeightFade :=
          smoothWeightFade * options.smoothWeightProgress;

        VAR
          precOk            := FALSE;
          subIter: CARDINAL := options.maxSubIter;
        BEGIN
          WHILE NOT precOk DO
            IF subIter = 0 THEN
              RAISE Arithmetic.Error(
                      NEW(Arithmetic.ErrorNoConvergence).init(
                        "Iteration for smoothing aborted after "
                          & Fmt.Int(options.maxSubIter) & " cycles."));
            ELSE
              DEC(subIter);
            END;
            WITH der = ComputeOptCritDiff(x) DO
              x := V.Sub(x, LA.LeastSquares(
                              der.second, ARRAY OF V.T{der.first})[0].x);
              precOk :=
                VT.Norm1(der.first) <= options.tol * RT.Abs(der.zeroth);
            END;
          END;
        END;

        PlotBase();
      END;

      IF Flag.Plot IN flags THEN PL.StopPage(); END;

      RETURN matching.splitParamVec(x);
    END;
  END WithPattern;


PROCEDURE BSplineWithPattern
  (         target                                 : S.T;
            dualSmooth, primalSmooth, dualVanishing: CARDINAL;
            translates                             : Range.T;
            numLevels                              : CARDINAL;
            smoothWeight                           : R.T;
            flags                                  : FlagSet;
   READONLY options                                : Options;  ):
  ARRAY [0 .. 1] OF FB.TBody RAISES {Arithmetic.Error} =
  VAR
    basis := WMBasis.InitBSpline(NEW(WMBasis.NonVanishing), dualSmooth,
                                 primalSmooth, dualVanishing);
    mc := WithPattern(target, basis, translates, numLevels, smoothWeight,
                      flags, options);
  BEGIN
    RETURN basis.getFilterBank(mc);
  END BSplineWithPattern;

BEGIN
END WaveletMatchSmooth.
