MODULE TestBSpline;

IMPORT LongRealBasic AS R;
IMPORT LongRealFmtLex AS RF;
IMPORT LongRealTrans AS RT;
IMPORT LongRealIntegerPower AS RIntPow;
IMPORT RandomDECSRC AS Rnd;
IMPORT LongRealSignal AS S;
IMPORT LongRealVectorFast AS V;
IMPORT LongRealComplexVectorTrans AS CVT;
IMPORT LongRealEigenSystem AS Eigen;

IMPORT LongRealBSplineWavelet AS BSpl;
IMPORT LongRealRefinableFunc AS Refn;

IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
IMPORT NADefinitions AS NA;

PROCEDURE ShowFilters () =
  <*FATAL BSpl.DifferentParity, Thread.Alerted, Wr.Failure*>
  BEGIN
    IO.Put(Fmt.FN("dual generator: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpl.GeneratorMask(2))}));
    IO.Put(Fmt.FN("dual wavelet: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpl.WaveletMask(2, 2))}));
  END ShowFilters;

PROCEDURE Reconstruction (hdual, gdual: S.T): S.T =
  VAR
    prod0 := gdual.alternate().convolve(hdual);
    prod1 := hdual.alternate().convolve(gdual);
  BEGIN
    RETURN prod0.superpose(prod1.negate());
  END Reconstruction;

PROCEDURE CheckPerfectReconstruction () =
  <*FATAL BSpl.DifferentParity, Thread.Alerted, Wr.Failure*>
  BEGIN
    FOR i := 0 TO 6 DO
      FOR j := i MOD 2 TO 8 BY 2 DO
        IO.Put(
          Fmt.FN("CDF-%s,%s: %s\n",
                 ARRAY OF
                   TEXT{Fmt.Int(i), Fmt.Int(j),
                        SF.Fmt(Reconstruction(BSpl.GeneratorMask(i),
                                              BSpl.WaveletMask(i, j)))}));
      END;
    END;
  END CheckPerfectReconstruction;

PROCEDURE ShowWavelets (h, g: S.T; levels: CARDINAL) =
  VAR
    twopow := RIntPow.Power(R.Two, levels);
    grid:=1.0D0/twopow;
    phi    := Refn.Refine(S.One, h, levels).scale(twopow);
    psi    := Refn.Refine(g, h, levels).scale(twopow);
    left   := FLOAT(psi.getFirst(), R.T) * grid;
    length := FLOAT(psi.getNumber(), R.T) * grid;
  BEGIN
    PL.SetEnvironment(left, left + length, -1.1D0, 1.1D0);
    PL.PlotLines(V.ArithSeq(psi.getNumber(), left, grid)^, psi.getData()^);
  END ShowWavelets;

PROCEDURE ShowBSplWavelets () =
  BEGIN
    ShowWavelets(BSpl.GeneratorMask(3), BSpl.WaveletMask(3, 9), 6);
  END ShowBSplWavelets;

PROCEDURE Test () =
  BEGIN
    PL.Init();
    CASE 2 OF
    | 0 => ShowFilters();
    | 1 => CheckPerfectReconstruction();
    | 2 => ShowBSplWavelets();
    ELSE
      <*ASSERT FALSE*>
    END;
    PL.Exit();
  END Test;

BEGIN
END TestBSpline.
