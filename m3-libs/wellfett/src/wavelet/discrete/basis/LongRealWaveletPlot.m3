MODULE LongRealWaveletPlot;

IMPORT LongRealBasic        AS R,
       LongRealIntegerPower AS RIntPow,
       LongRealSignal       AS S,
       LongRealVector       AS V,
       LongRealVectorFast   AS VR,
       Arithmetic           AS Arith;

IMPORT LongRealRefinableFunc AS Refn, LongRealDyadicFilterBank AS FB;

IMPORT LongRealSignalFmtLex AS SF, LongRealPLPlot AS PL;

TYPE
  Basis = {primal, dual};
  Filter = [0 .. 1];
  FilterBank = ARRAY Basis, Filter OF S.T;
  BasisFunctions = ARRAY Basis, Filter OF S.T;

PROCEDURE ExtractRefinementMasks (READONLY bank: ARRAY Basis, Filter OF S.T; ):
  ARRAY Basis OF S.T =
  BEGIN
    RETURN ARRAY Basis OF
             S.T{bank[Basis.primal, 0].scale(R.Two),
                 bank[Basis.dual, 0].scale(R.Two)};
  END ExtractRefinementMasks;

PROCEDURE ComputeBasisFunctions (READONLY bank: ARRAY Basis, Filter OF S.T;
                                 READONLY refn     : ARRAY Basis OF S.T;
                                          numlevels: CARDINAL            ):
  BasisFunctions =
  VAR basis: BasisFunctions;
  BEGIN
    FOR b := FIRST(basis) TO LAST(basis) DO
      FOR f := FIRST(basis[b]) TO LAST(basis[b]) DO
        basis[b, f] := Refn.Refine(bank[b, f], refn[b], numlevels);
      END;
    END;
    RETURN basis;
  END ComputeBasisFunctions;

PROCEDURE PlotOrthogonal (h: S.T; numlevels: CARDINAL) =
  BEGIN
    PlotBiorthogonal(h, h.adjoint().translate(1), numlevels);
  END PlotOrthogonal;

PROCEDURE PlotBiorthogonal (hDual, gDual: S.T; numlevels: CARDINAL) =
  VAR
    dual := ARRAY Filter OF S.T{hDual, gDual};
    bank := FilterBank{FB.Complement(dual), dual};
  BEGIN
    PlotBank(bank, ExtractRefinementMasks(bank), numlevels);
  END PlotBiorthogonal;

PROCEDURE PlotBiorthogonalYLim (hDual, gDual: S.T;
                                numlevels   : CARDINAL;
                                ymin, ymax  : R.T       ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    dual  := ARRAY Filter OF S.T{hDual, gDual};
    bank  := FilterBank{FB.Complement(dual), dual};
    refn  := ExtractRefinementMasks(bank);
    grid  := R.One / RIntPow.Power(R.Two, numlevels);
    basis := ComputeBasisFunctions(bank, refn, numlevels);
  BEGIN
    DoPlot(basis, ymin, ymax, grid);
  END PlotBiorthogonalYLim;

PROCEDURE PlotAny (refnPrimal, refnDual          : S.T;
                   hPrimal, gPrimal, hDual, gDual: S.T;
                   numlevels                     : CARDINAL) =
  BEGIN
    PlotBank(FilterBank{ARRAY Filter OF S.T{hPrimal, gPrimal},
                        ARRAY Filter OF S.T{hDual, gDual}},
             ARRAY Basis OF S.T{refnPrimal, refnDual}, numlevels);
  END PlotAny;

PROCEDURE PlotAnyYLim (refnPrimal, refnDual          : S.T;
                       hPrimal, gPrimal, hDual, gDual: S.T;
                       numlevels                     : CARDINAL;
                       ymin, ymax                    : R.T       ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    grid := R.One / RIntPow.Power(R.Two, numlevels);
    basis := ComputeBasisFunctions(
               FilterBank{ARRAY Filter OF S.T{hPrimal, gPrimal},
                          ARRAY Filter OF S.T{hDual, gDual}},
               ARRAY Basis OF S.T{refnPrimal, refnDual}, numlevels);
  BEGIN
    DoPlot(basis, ymin, ymax, grid);
  END PlotAnyYLim;

PROCEDURE PlotBank (READONLY bank     : FilterBank;
                    READONLY refn     : ARRAY Basis OF S.T;
                             numlevels: CARDINAL            ) =
  <* FATAL Arith.Error *>        (*Power can't fail for reals*)
  VAR
    grid  := R.One / RIntPow.Power(R.Two, numlevels);
    basis := ComputeBasisFunctions(bank, refn, numlevels);
    ymin := MIN(MIN(VR.Min(basis[Basis.primal, 0].getData()^),
                    VR.Min(basis[Basis.primal, 1].getData()^)),
                MIN(VR.Min(basis[Basis.dual, 0].getData()^),
                    VR.Min(basis[Basis.dual, 1].getData()^)));
    ymax := MAX(MAX(VR.Max(basis[Basis.primal, 0].getData()^),
                    VR.Max(basis[Basis.primal, 1].getData()^)),
                MAX(VR.Max(basis[Basis.dual, 0].getData()^),
                    VR.Max(basis[Basis.dual, 1].getData()^)));
  BEGIN
    DoPlot(basis, ymin, ymax, grid);
  END PlotBank;

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
  END DoPlot;

BEGIN
END LongRealWaveletPlot.
