MODULE PlotWavelets;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorFast AS VFs;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;
IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealDyadicFilterBank AS FB;

IMPORT LongRealBSplineWavelet AS BSpl;

(*IMPORT LongRealFmtLex AS RF;*)
IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;
IMPORT Fmt;
(*IMPORT IO, Fmt, Wr, Thread;*)

IMPORT NADefinitions AS NA;

IMPORT TestMatchWavelet;

TYPE Box = RECORD horizontal, vertical: ARRAY [0 .. 1] OF R.T;  END;


CONST psOutput = TRUE;

PROCEDURE PLInitPS (fileName: TEXT; ) =
  BEGIN
    IF psOutput THEN
      PL.SetDevice("ps");
      PL.SetFileName(
        "/home/thielema/projects/paper/matchedwavelet/figures/" & fileName
          & ".eps");

      PL.Init();

      PL.SetPenWidth(20);
      PL.SetCharacterHeight(R.Zero, 1.5D0);
    ELSE
      PL.Init();
    END;
  END PLInitPS;

PROCEDURE PlotSignal (func: S.T; READONLY bounds: Box; grid: R.T; ) =
  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(bounds.horizontal[0], bounds.horizontal[1],
                      bounds.vertical[0], bounds.vertical[1]);

    PL.SetFGColorDiscr(2);
    PL.PlotLines(V.ArithSeq(func.getNumber(),
                            FLOAT(func.getFirst(), R.T) * grid, grid)^,
                 func.getData()^);
  END PlotSignal;


PROCEDURE PlotApproximation (fileName                              : TEXT;
                             target                                : S.T;
                             refineMask, generatorMask, waveletMask: S.T;
                             numLevels, numTranslates: CARDINAL; ) =
  VAR
    unit := IIntPow.MulPower(1, 2, numLevels);
    grid := R.Half / FLOAT(unit, R.T);
    approx := TestMatchWavelet.MatchPattern(
                target, refineMask, generatorMask, waveletMask, numLevels,
                -numTranslates, 2 * numTranslates);

    bounds := Box{horizontal :=
                  ARRAY OF
                    R.T{FLOAT(approx.approx.getFirst(), R.T) * grid,
                        FLOAT(approx.approx.getLast(), R.T) * grid},
                  vertical := ARRAY OF
                                R.T{1.1D0 * VFs.Min(target.getData()^),
                                    1.1D0 * VFs.Max(target.getData()^)}};
    bigBounds := Box{horizontal := bounds.horizontal, vertical :=
                     ARRAY OF
                       R.T{bounds.vertical[0] / approx.wavelet0Amp,
                           bounds.vertical[1] / approx.wavelet0Amp}};

    paddedTarget := target.clip(target.getFirst() - 3 * unit,
                                target.getNumber() + 6 * unit);

    waveletLiftedMask := waveletMask.superpose(
                           generatorMask.upConvolve(
                             approx.lift.scale(R.One / approx.wavelet0Amp),
                             2));
    waveletLifted := Refn.Refine(waveletLiftedMask, refineMask, numLevels);
    complement    := FB.GetComplement(FB.T{generatorMask, waveletLifted});

  BEGIN
    PLInitPS(fileName & "-orig");
    PlotSignal(paddedTarget, bounds, grid);
    PL.Exit();

    (*'approx' and 'lifted' should be the same*)
    PLInitPS(fileName & "-approx");
    PlotSignal(approx.approx, bounds, grid);
    PL.Exit();

    PLInitPS(fileName & "-lifted");
    PlotSignal(waveletLifted, bigBounds, grid);
    PL.Exit();

    PLInitPS(fileName & "-compl-gen");
    PlotSignal(complement[0], bigBounds, grid);
    PL.Exit();
  END PlotApproximation;

PROCEDURE PlotLiftingBasis (fileName                              : TEXT;
                            refineMask, generatorMask, waveletMask: S.T;
                            numLevels, numTranslates: CARDINAL; ) =
  VAR
    unit       := IIntPow.MulPower(2, 2, numLevels);
    grid       := R.One / FLOAT(unit, R.T);
    generator  := Refn.Refine(generatorMask, refineMask, numLevels);
    wavelet    := Refn.Refine(waveletMask, refineMask, numLevels);
    firstTrans := -numTranslates;
    lastTrans  := numTranslates - 1;
    bounds := Box{
                horizontal :=
                ARRAY OF
                  R.T{FLOAT(generator.getFirst() + firstTrans * unit, R.T)
                        * grid, FLOAT(generator.getLast()
                                        + lastTrans * unit, R.T) * grid},
                vertical := ARRAY OF
                              R.T{1.1D0 * VFs.Min(wavelet.getData()^),
                                  1.1D0 * VFs.Max(wavelet.getData()^)}};

  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
    PLInitPS(fileName);

    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(bounds.horizontal[0], bounds.horizontal[1],
                      bounds.vertical[0], bounds.vertical[1]);

    PL.SetFGColorDiscr(2);
    FOR j := firstTrans TO lastTrans DO
      PL.PlotLines(V.ArithSeq(generator.getNumber(),
                              FLOAT(generator.getFirst() + j * unit, R.T)
                                * grid, grid)^, generator.getData()^);
    END;

    PL.SetLineStyle(PL.LineStyle.longDash);
    PL.SetFGColorDiscr(3);
    PL.PlotLines(V.ArithSeq(wavelet.getNumber(),
                            FLOAT(wavelet.getFirst(), R.T) * grid, grid)^,
                 wavelet.getData()^);
    PL.Exit();
  END PlotLiftingBasis;

PROCEDURE PlotWavelet (fileName                  : TEXT;
                       generatorMask, waveletMask: S.T;
                       numLevels                 : CARDINAL;
                       bounds                    : REF Box    := NIL; ) =
  VAR
    unit := IIntPow.MulPower(1, 2, numLevels);
    grid := R.Half / FLOAT(unit, R.T);
    func := Refn.Refine(waveletMask, generatorMask, numLevels);

  BEGIN
    PLInitPS(fileName);

    IF bounds = NIL THEN
      bounds := NEW(REF Box,
                    horizontal := ARRAY OF
                                    R.T{FLOAT(func.getFirst(), R.T) * grid,
                                        FLOAT(func.getLast(), R.T) * grid},
                    vertical := ARRAY OF
                                  R.T{VFs.Min(func.getData()^),
                                      VFs.Max(func.getData()^)});
    END;

    PlotSignal(func, bounds^, grid);
    PL.Exit();
  END PlotWavelet;


PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  CONST
    numLevels         = 7;
    cdfDualSmooth     = 3;
    cdfPrimalSmooth   = 5;
    cdf31PrimalSmooth = 1;
  VAR
    vanAtom := NEW(S.T).fromArray(ARRAY OF R.T{R.Half, R.Zero, -R.Half});
    unit    := IIntPow.MulPower(1, 2, numLevels);
    cdfName := Fmt.FN("cdf-%s-%s", ARRAY OF
                                     TEXT{Fmt.Int(cdfDualSmooth),
                                          Fmt.Int(cdfPrimalSmooth)});
    (*Translate by one step to match Daubechies' figures.*)
    cdfDual := FB.T{BSpl.GeneratorMask(cdfDualSmooth).scale(
                      R.Two).translate(-1),
                    BSpl.WaveletMask(cdfDualSmooth, cdfPrimalSmooth).scale(
                      R.Two).translate(1)};
    cdfPrimal := FB.GetComplement(cdfDual);
    cdfBounds := NEW(REF Box, horizontal := ARRAY OF R.T{-4.0D0, 5.0D0},
                     vertical := ARRAY OF R.T{-3.0D0, 3.0D0});

    cdfDualGeneratorVanMask := SIntPow.MulPower(cdfDual[0], vanAtom,
                                                cdfPrimalSmooth).translate(
                                 2 - cdfPrimalSmooth - cdfDualSmooth);
    cdf31DualGeneratorVanMask := SIntPow.MulPower(
                                   cdfDual[0], vanAtom, cdf31PrimalSmooth).translate(
                                   2 - cdf31PrimalSmooth - cdfDualSmooth);
    cdf31DualWaveletMask := BSpl.WaveletMask(
                              cdfDualSmooth, cdf31PrimalSmooth).scale(
                              R.Two).translate(1);

    (*
        cdf4Dual := FB.T{BSpl.GeneratorMask(4).scale(R.Two).translate(-1),
                         BSpl.WaveletMask(4, 4).scale(R.Two).translate(1)};
    *)
    daub2 := NEW(S.T).fromArray(
               ARRAY OF
                 R.T{-0.129409522550921D0, 0.224143868041857D0,
                     0.836516303737469D0, 0.48296291314469D0});
    daub4 := NEW(S.T).fromArray(
               ARRAY OF
                 R.T{-0.0105974017849973D0, 0.0328830116669829D0,
                     0.030841381835987D0, -0.187034811718881D0,
                     -0.0279837694169838D0, 0.63088076792959D0,
                     0.714846570552542D0, 0.230377813308855D0});

  BEGIN
    (*doubled generator amplitude for nicer appearance*)
    PlotLiftingBasis("liftingbasis", cdfDual[0].translate(2),
                     cdf31DualGeneratorVanMask.scale(R.Two),
                     cdf31DualWaveletMask.translate(-2), numLevels, 8);

    (*The problem should be completely symmetric, but it isn't.  When the
       ramp is shifted to the left by 2..3 then lifting filter is
       approximately symmetric.*)
    PlotApproximation(
      "ramp",
      NEW(S.T).fromArray(
        V.ArithSeq(2048 + 1, -1.0D0, 2.0D0 / 2048.0D0)^, unit - 1024),
      cdfDual[0].translate(2), cdf31DualGeneratorVanMask,
      cdf31DualWaveletMask.translate(-2), numLevels, 8);

    PlotApproximation(
      "ramp5van",
      NEW(S.T).fromArray(
        V.ArithSeq(2048 + 1, -1.0D0, 2.0D0 / 2048.0D0)^, unit - 1024),
      cdfDual[0].translate(2), cdfDualGeneratorVanMask,
      cdfDual[0].translate(-2), numLevels, 8);

    PlotWavelet(
      cdfName & "-dual-gen", cdfDual[0], cdfDual[0], numLevels, cdfBounds);
    PlotWavelet(
      cdfName & "-dual-wav", cdfDual[0], cdfDual[1], numLevels, cdfBounds);
    PlotWavelet(cdfName & "-primal-gen", cdfPrimal[0], cdfPrimal[0],
                numLevels, cdfBounds);
    PlotWavelet(cdfName & "-primal-wav", cdfPrimal[0], cdfPrimal[1],
                numLevels, cdfBounds);
    PlotWavelet("daub2", daub2.adjoint().scale(RT.SqRtTwo).translate(1),
                daub2.alternate(), numLevels, cdfBounds);
    PlotWavelet("daub4", daub4.adjoint().scale(RT.SqRtTwo).translate(1),
                daub4.alternate(), numLevels, cdfBounds);
  END Test;

BEGIN
END PlotWavelets.
