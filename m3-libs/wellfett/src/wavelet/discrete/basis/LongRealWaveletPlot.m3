MODULE LongRealWaveletPlot;

IMPORT LongRealBasic AS R;
IMPORT LongRealIntegerPower AS RIntPow;
IMPORT LongRealSignal AS S;
IMPORT LongRealVectorFast AS V;

IMPORT LongRealRefinableFunc AS Refn;

IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;

FROM NADefinitions IMPORT Error;

TYPE
  Basis = {primal, dual};
  Filter = {lowpass, highpass};
  BasisFunctions = ARRAY Basis, Filter OF S.T;

PROCEDURE ComputeBasisFunctions (hdual, gdual: S.T; levels: CARDINAL):
  BasisFunctions =
  VAR
    hprimal  := gdual.alternate();
    gprimal  := hdual.alternate();
    hdual2   := hdual.scale(R.Two);
    hprimal2 := hprimal.scale(R.Two);

  BEGIN
    RETURN BasisFunctions{ARRAY Filter OF
                            S.T{Refn.Refine(hprimal, hprimal2, levels),
                                Refn.Refine(gprimal, hprimal2, levels)},
                          ARRAY Filter OF
                            S.T{Refn.Refine(hdual, hdual2, levels),
                                Refn.Refine(gdual, hdual2, levels)}};
  END ComputeBasisFunctions;

PROCEDURE PlotWavelets (hdual, gdual: S.T; levels: CARDINAL) =
  VAR
    grid  := 1.0D0 / RIntPow.Power(R.Two, levels);
    basis := ComputeBasisFunctions(hdual, gdual, levels);

    ymin := MIN(
              MIN(V.Min(basis[Basis.primal, Filter.lowpass].getData()^),
                  V.Min(basis[Basis.primal, Filter.highpass].getData()^)),
              MIN(V.Min(basis[Basis.dual, Filter.lowpass].getData()^),
                  V.Min(basis[Basis.dual, Filter.highpass].getData()^)));
    ymax := MAX(
              MAX(V.Max(basis[Basis.primal, Filter.lowpass].getData()^),
                  V.Max(basis[Basis.primal, Filter.highpass].getData()^)),
              MAX(V.Max(basis[Basis.dual, Filter.lowpass].getData()^),
                  V.Max(basis[Basis.dual, Filter.highpass].getData()^)));

  BEGIN
    DoPlot(basis, ymin, ymax, grid);
  END PlotWavelets;

PROCEDURE PlotWaveletsYLim (hdual, gdual: S.T;
                            levels      : CARDINAL;
                            ymin, ymax  : R.T       ) =
  VAR
    grid  := 1.0D0 / RIntPow.Power(R.Two, levels);
    basis := ComputeBasisFunctions(hdual, gdual, levels);
  BEGIN
    DoPlot(basis, ymin, ymax, grid);
  END PlotWaveletsYLim;

TYPE Interval = RECORD left, right: R.T END;

PROCEDURE GetSignalInterval (x: S.T; grid: R.T): Interval =
  BEGIN
    RETURN Interval{FLOAT(x.getFirst(), R.T) * grid,
                    FLOAT(x.getLast(), R.T) * grid};
  END GetSignalInterval;

PROCEDURE DoPlot (basis: BasisFunctions; ymin, ymax: R.T; grid: R.T) =
  VAR
    bounds : ARRAY Basis, Filter OF Interval;
    boundsa: ARRAY Basis OF Interval;
  (*PlotLines may complain about inconsistent vector sizes but we won't
     give it a reason for complaints.*)
  <*FATAL Error*>
  BEGIN
    FOR b := FIRST(basis) TO LAST(basis) DO
      FOR f := FIRST(basis[b]) TO LAST(basis[b]) DO
        bounds[b, f] := GetSignalInterval(basis[b, f], grid);
      END;
      boundsa[b].left := MIN(bounds[b, Filter.lowpass].left,
                             bounds[b, Filter.highpass].left);
      boundsa[b].right := MAX(bounds[b, Filter.lowpass].right,
                              bounds[b, Filter.highpass].right);
    END;

    PL.SetSubWindows(2, 2);
    FOR f := FIRST(basis[Basis.primal]) TO LAST(basis[Basis.primal]) DO
      FOR b := FIRST(basis) TO LAST(basis) DO
        PL.SetFGColorDiscr(1);
        PL.SetEnvironment(boundsa[b].left, boundsa[b].right, ymin, ymax);
        PL.SetFGColorDiscr(2);
        PL.PlotLines(
          V.ArithSeq(basis[b, f].getNumber(), bounds[b, f].left, grid)^,
          basis[b, f].getData()^);
      END;
    END;
  END DoPlot;

BEGIN
END LongRealWaveletPlot.
