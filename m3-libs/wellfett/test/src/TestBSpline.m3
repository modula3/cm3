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

PROCEDURE ShowWavelets (hdual, gdual: S.T; levels: CARDINAL) =
  CONST
    ymin = -1.5D0;
    ymax = 1.5D0;
  VAR
    twopow  := RIntPow.Power(R.Two, levels);
    grid    := 1.0D0 / twopow;
    hprimal := gdual.alternate();
    gprimal := hdual.alternate();

    phiprimal      := Refn.Refine(hprimal, hprimal, levels).scale(twopow);
    psiprimal      := Refn.Refine(gprimal, hprimal, levels).scale(twopow);
    leftphiprimal  := FLOAT(phiprimal.getFirst(), R.T) * grid;
    rightphiprimal := FLOAT(phiprimal.getLast(), R.T) * grid;
    leftpsiprimal  := FLOAT(psiprimal.getFirst(), R.T) * grid;
    rightpsiprimal := FLOAT(psiprimal.getLast(), R.T) * grid;
    leftprimal     := MIN(leftphiprimal, leftpsiprimal);
    rightprimal    := MAX(rightphiprimal, rightpsiprimal);

    phidual      := Refn.Refine(hdual, hdual, levels).scale(twopow);
    psidual      := Refn.Refine(gdual, hdual, levels).scale(twopow);
    leftphidual  := FLOAT(phidual.getFirst(), R.T) * grid;
    rightphidual := FLOAT(phidual.getLast(), R.T) * grid;
    leftpsidual  := FLOAT(psidual.getFirst(), R.T) * grid;
    rightpsidual := FLOAT(psidual.getLast(), R.T) * grid;
    leftdual     := MIN(leftphidual, leftpsidual);
    rightdual    := MAX(rightphidual, rightpsidual);

  BEGIN
    PL.SubPlots(2, 2);
    PL.SetEnvironment(leftprimal, rightprimal, ymin, ymax);
    PL.PlotLines(V.ArithSeq(phiprimal.getNumber(), leftphiprimal, grid)^,
                 phiprimal.getData()^);
    PL.SetEnvironment(leftdual, rightdual, ymin, ymax);
    PL.PlotLines(V.ArithSeq(phidual.getNumber(), leftphidual, grid)^,
                 phidual.getData()^);

    PL.SetEnvironment(leftprimal, rightprimal, ymin, ymax);
    PL.PlotLines(V.ArithSeq(psiprimal.getNumber(), leftpsiprimal, grid)^,
                 psiprimal.getData()^);
    PL.SetEnvironment(leftdual, rightdual, ymin, ymax);
    PL.PlotLines(V.ArithSeq(psidual.getNumber(), leftpsidual, grid)^,
                 psidual.getData()^);
  END ShowWavelets;

PROCEDURE ShowBSplWavelets () =
  <*FATAL BSpl.DifferentParity*>
  BEGIN
    ShowWavelets(BSpl.GeneratorMask(2), BSpl.WaveletMask(2,2), 6);
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
