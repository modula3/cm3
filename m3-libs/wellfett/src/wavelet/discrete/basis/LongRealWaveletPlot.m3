MODULE LongRealWaveletPlot;

IMPORT LongRealBasic        AS R,
       LongRealIntegerPower AS RIntPow,
       LongRealSignal       AS S,
       LongRealVector       AS V,
       LongRealVectorFast   AS VFs,
       LongRealMatrix       AS M,
       Arithmetic           AS Arith;

IMPORT LongRealRefinableFunction AS Refn, LongRealDyadicFilterBank AS FB;

IMPORT LongRealSignalFmtLex AS SF, LongRealPLPlot AS PL;

TYPE
  Basis = {primal, dual};
  Filter = [0 .. 1];
  FilterBank = ARRAY Basis, Filter OF S.T;
  BasisFunctions = ARRAY Basis, Filter OF S.T;

PROCEDURE ExtractRefinementMasks (READONLY bank: FilterBank; ):
  ARRAY Basis OF S.T =
  BEGIN
    RETURN ARRAY Basis OF
             S.T{bank[Basis.primal, 0].scale(R.Two),
                 bank[Basis.dual, 0].scale(R.Two)};
  END ExtractRefinementMasks;

PROCEDURE ComputeBasisFunctions (READONLY bank     : FilterBank;
                                 READONLY refn     : ARRAY Basis OF S.T;
                                          numLevels: CARDINAL;           ):
  BasisFunctions =
  VAR basis: BasisFunctions;
  BEGIN
    FOR b := FIRST(basis) TO LAST(basis) DO
      FOR f := FIRST(basis[b]) TO LAST(basis[b]) DO
        basis[b, f] := Refn.Refine(bank[b, f], refn[b], numLevels);
      END;
    END;
    RETURN basis;
  END ComputeBasisFunctions;

PROCEDURE Orthogonal (lp: S.T; numLevels: CARDINAL; ) =
  BEGIN
    Biorthogonal(lp, FB.OrthogonalLowPassToHighPass(lp), numLevels);
  END Orthogonal;

PROCEDURE Biorthogonal (lpDual, hpDual: S.T; numLevels: CARDINAL; ) =
  VAR
    dual := ARRAY Filter OF S.T{lpDual, hpDual};
    bank := FilterBank{FB.DualToPrimal(dual), dual};
  BEGIN
    Bank(bank, ExtractRefinementMasks(bank), numLevels);
  END Biorthogonal;

PROCEDURE BiorthogonalYLim
  (lpDual, hpDual: S.T; numLevels: CARDINAL; ymin, ymax: R.T; ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    dual  := ARRAY Filter OF S.T{lpDual, hpDual};
    bank  := FilterBank{FB.DualToPrimal(dual), dual};
    refn  := ExtractRefinementMasks(bank);
    grid  := R.One / RIntPow.Power(R.Two, numLevels);
    basis := ComputeBasisFunctions(bank, refn, numLevels);
  BEGIN
    Do(basis, ymin, ymax, grid);
  END BiorthogonalYLim;

PROCEDURE Any (refnPrimal, refnDual              : S.T;
               lpPrimal, hpPrimal, lpDual, hpDual: S.T;
               numLevels                         : CARDINAL; ) =
  BEGIN
    Bank(FilterBank{ARRAY Filter OF S.T{lpPrimal, hpPrimal},
                    ARRAY Filter OF S.T{lpDual, hpDual}},
         ARRAY Basis OF S.T{refnPrimal, refnDual}, numLevels);
  END Any;

PROCEDURE AnyYLim (refnPrimal, refnDual              : S.T;
                   lpPrimal, hpPrimal, lpDual, hpDual: S.T;
                   numLevels                         : CARDINAL;
                   ymin, ymax                        : R.T       ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    grid := R.One / RIntPow.Power(R.Two, numLevels);
    basis := ComputeBasisFunctions(
               FilterBank{ARRAY Filter OF S.T{lpPrimal, hpPrimal},
                          ARRAY Filter OF S.T{lpDual, hpDual}},
               ARRAY Basis OF S.T{refnPrimal, refnDual}, numLevels);
  BEGIN
    Do(basis, ymin, ymax, grid);
  END AnyYLim;

PROCEDURE Bank (READONLY bank     : FilterBank;
                READONLY refn     : ARRAY Basis OF S.T;
                         numLevels: CARDINAL            ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    grid  := R.One / RIntPow.Power(R.Two, numLevels);
    basis := ComputeBasisFunctions(bank, refn, numLevels);
    ymin := MIN(MIN(VFs.Min(basis[Basis.primal, 0].getData()^),
                    VFs.Min(basis[Basis.primal, 1].getData()^)),
                MIN(VFs.Min(basis[Basis.dual, 0].getData()^),
                    VFs.Min(basis[Basis.dual, 1].getData()^)));
    ymax := MAX(MAX(VFs.Max(basis[Basis.primal, 0].getData()^),
                    VFs.Max(basis[Basis.primal, 1].getData()^)),
                MAX(VFs.Max(basis[Basis.dual, 0].getData()^),
                    VFs.Max(basis[Basis.dual, 1].getData()^)));
  BEGIN
    Do(basis, ymin, ymax, grid);
  END Bank;

TYPE Interval = RECORD left, right: R.T END;

PROCEDURE GetSignalInterval (x: S.T; grid: R.T; ): Interval =
  BEGIN
    RETURN Interval{FLOAT(x.getFirst(), R.T) * grid,
                    FLOAT(x.getLast(), R.T) * grid};
  END GetSignalInterval;

PROCEDURE Do (basis: BasisFunctions; ymin, ymax: R.T; grid: R.T) =
  VAR
    bounds : ARRAY Basis, Filter OF Interval;
    boundsa: ARRAY Basis OF Interval;
  (* PlotLines may complain about inconsistent vector sizes but we won't
     give it a reason for complaints.*)
  BEGIN
    FOR b := FIRST(basis) TO LAST(basis) DO
      FOR f := FIRST(basis[b]) TO LAST(basis[b]) DO
        bounds[b, f] := GetSignalInterval(basis[b, f], grid);
      END;
      boundsa[b].left := MIN(bounds[b, 0].left, bounds[b, 1].left);
      boundsa[b].right := MAX(bounds[b, 0].right, bounds[b, 1].right);
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
  END Do;


PROCEDURE Frame (READONLY abscissa       : V.TBody;
                 READONLY frame          : M.TBody;
                 READONLY wavelet, target: V.TBody; ) =

  CONST normalizeHeight = TRUE;

  PROCEDURE PlotLines (READONLY x, y: V.TBody; ) =
    BEGIN
      IF normalizeHeight THEN
        PL.PlotLines(
          x, VFs.Scale(y, R.One / MAX(VFs.Max(y), -VFs.Min(y)))^);
      ELSE
        PL.PlotLines(x, y);
      END;
    END PlotLines;

  VAR yMin, yMax: R.T;

  BEGIN
    IF normalizeHeight THEN
      yMin := -1.1D0;
      yMax := 1.1D0;
    ELSE
      VAR
        waveYMin := MIN(VFs.Min(frame[0]), VFs.Min(wavelet));
        waveYMax := MAX(VFs.Max(frame[0]), VFs.Max(wavelet));
      BEGIN
        yMin := 1.1D0 * MIN(VFs.Min(target), waveYMin);
        yMax := 1.1D0 * MAX(VFs.Max(target), waveYMax);
      END;
    END;

    PL.SetFGColorDiscr(7);
    PL.SetEnvironment(
      abscissa[FIRST(abscissa)], abscissa[LAST(abscissa)], yMin, yMax,
      axis := PL.TileSet{PL.Tile.Box, PL.Tile.Ticks, PL.Tile.Axes,
                         PL.Tile.GridMajor, PL.Tile.GridMinor});

    PL.SetFGColorDiscr(1);
    PlotLines(abscissa, wavelet);
    PL.SetFGColorDiscr(2);
    FOR j := FIRST(frame) TO LAST(frame) DO
      PlotLines(abscissa, frame[j]);
    END;

    PL.SetFGColorDiscr(3);
    PlotLines(abscissa, target);
  END Frame;


BEGIN
END LongRealWaveletPlot.
