MODULE PlotWavelets;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorFast AS VFs;

IMPORT LongRealSignal AS S;
IMPORT LongRealRefinableFunc AS Refn;

IMPORT LongRealBSplineWavelet AS BSpl;

(*IMPORT LongRealFmtLex AS RF;*)
IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;
(*IMPORT IO, Fmt, Wr, Thread;*)

IMPORT NADefinitions AS NA;


PROCEDURE PlotWavelet (generator, wavelet: S.T; numLevels: CARDINAL; ) =
  VAR
    unit  := IIntPow.MulPower(1, 2, numLevels);
    grid  := R.Half / FLOAT(unit, R.T);
    func  := Refn.Refine(wavelet, generator, numLevels);
    left  := FLOAT(func.getFirst(), R.T) * grid;
    right := FLOAT(func.getLast(), R.T) * grid;

  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
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
  CONST numLevels = 8;
  VAR
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
    PlotWavelet(BSpl.GeneratorMask(2).scale(R.Two),
                BSpl.WaveletMask(2, 2).scale(-R.Two), numLevels);
    PlotWavelet(daub2.adjoint().scale(RT.SqRtTwo),
                daub2.alternate().translate(1), numLevels);
    PlotWavelet(daub4.adjoint().scale(RT.SqRtTwo),
                daub4.alternate().translate(1), numLevels);
  END Test;

BEGIN
END PlotWavelets.
