MODULE PlotWavelets;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorFast AS VFs;

IMPORT LongRealSignal AS S;
IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealDyadicFilterBank AS FB;

IMPORT LongRealBSplineWavelet AS BSpl;

(*IMPORT LongRealFmtLex AS RF;*)
IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;
IMPORT Fmt;
(*IMPORT IO, Fmt, Wr, Thread;*)

IMPORT NADefinitions AS NA;


PROCEDURE PlotWavelet (fileName          : TEXT;
                       generator, wavelet: S.T;
                       numLevels         : CARDINAL; ) =
  VAR
    unit  := IIntPow.MulPower(1, 2, numLevels);
    grid  := R.Half / FLOAT(unit, R.T);
    func  := Refn.Refine(wavelet, generator, numLevels);
    left  := FLOAT(func.getFirst(), R.T) * grid;
    right := FLOAT(func.getLast(), R.T) * grid;

  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
    PL.SetDevice("ps");
    PL.SetFileName(
      "/home/thielema/projects/paper/matchedwavelet/figures/" & fileName);
    PL.Init();

    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      left, right, VFs.Min(func.getData()^), VFs.Max(func.getData()^));

    PL.SetFGColorDiscr(2);
    PL.PlotLines(V.ArithSeq(func.getNumber(),
                            FLOAT(func.getFirst(), R.T) * grid, grid)^,
                 func.getData()^);

    PL.Exit();
  END PlotWavelet;


PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  CONST
    numLevels    = 7;
    dualSmooth   = 2;
    primalSmooth = 4;
  VAR
    cdfName := Fmt.FN(
                 "cdf-%s-%s",
                 ARRAY OF TEXT{Fmt.Int(dualSmooth), Fmt.Int(primalSmooth)});
    (*Translate by one step to match Daubechies' figures.*)
    cdfDual := FB.T{
                 BSpl.GeneratorMask(dualSmooth).scale(R.Two).translate(-1),
                 BSpl.WaveletMask(dualSmooth, primalSmooth).scale(
                   R.Two).translate(1)};
    cdfPrimal := FB.DualToPrimal(cdfDual);
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
    PlotWavelet(
      cdfName & "-dual-gen.eps", cdfDual[0], cdfDual[0], numLevels);
    PlotWavelet(
      cdfName & "-dual-wav.eps", cdfDual[0], cdfDual[1], numLevels);
    PlotWavelet(
      cdfName & "-primal-gen.eps", cdfPrimal[0], cdfPrimal[0], numLevels);
    PlotWavelet(
      cdfName & "-primal-wav.eps", cdfPrimal[0], cdfPrimal[1], numLevels);
    PlotWavelet(
      "daub2.eps", daub2.adjoint().scale(RT.SqRtTwo).translate(1),
      daub2.alternate(), numLevels);
    PlotWavelet(
      "daub4.eps", daub4.adjoint().scale(RT.SqRtTwo).translate(1),
      daub4.alternate(), numLevels);
  END Test;

BEGIN
END PlotWavelets.
