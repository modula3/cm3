MODULE LongRealWaveletPlot;

IMPORT LongRealBasic AS R;
IMPORT LongRealIntegerPower AS RIntPow;
IMPORT LongRealSignal AS S;
IMPORT LongRealVectorFast AS V;

IMPORT LongRealRefinableFunc AS Refn;

IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;

FROM NADefinitions IMPORT Error;

PROCEDURE PlotWavelets (hdual, gdual: S.T; levels: CARDINAL) =
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

    ymin := MIN(
              MIN(V.Min(phiprimal.getData()^), V.Min(psiprimal.getData()^)),
              MIN(V.Min(phidual.getData()^), V.Min(psidual.getData()^)));
    ymax := MAX(
              MAX(V.Max(phiprimal.getData()^), V.Max(psiprimal.getData()^)),
              MAX(V.Max(phidual.getData()^), V.Max(psidual.getData()^)));

  BEGIN
    TRY
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
    EXCEPT
    | Error (err) => EVAL err;
      <*ASSERT FALSE*>(*PlotLines may complain about inconsistent vector
                         sizes but we give no reason for it.*)
    END;
  END PlotWavelets;

BEGIN
END LongRealWaveletPlot.
