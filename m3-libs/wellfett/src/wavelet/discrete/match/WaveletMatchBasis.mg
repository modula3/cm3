GENERIC MODULE WaveletMatchBasis(R, RIntPow, S, SIntPow, FB, WMGrad,
                                 BSpline, WPlot);

IMPORT Arithmetic;

FROM WMGrad IMPORT Parameters;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        getRefineMask             := GetRefineMask;
        getLSGeneratorMask        := GetLSGeneratorMask;
        getLSWavelet0Mask         := GetLSWavelet0Mask;
        getLiftedWaveletMaskNoVan := GetLiftedWaveletMaskNoVan;
        getShapeWaveletMask       := GetShapeWaveletMask;
      END;

  Standard = T BRANDED OBJECT
             OVERRIDES
               plotBase      := StdPlotBase;
               getFilterBank := StdGetFilterBank;
             END;

  NonVanishing = T BRANDED OBJECT
                 OVERRIDES
                   plotBase      := NoVanPlotBase;
                   getFilterBank := NoVanGetFilterBank;
                 END;


PROCEDURE GetLiftedWaveletMaskNoVan (SELF: T; READONLY mc: Parameters; ):
  S.T =
  VAR
    hpDif := SELF.lpNoVan.upConvolve(
               mc.lift.scale(R.One / mc.wavelet0Amp), 2);
  BEGIN
    RETURN SELF.hpNoVan.superpose(hpDif);
  END GetLiftedWaveletMaskNoVan;


PROCEDURE GetRefineMask (SELF: T; ): S.T =
  BEGIN
    RETURN SELF.lp;
  END GetRefineMask;

PROCEDURE GetLSGeneratorMask (SELF: T; ): S.T =
  BEGIN
    RETURN SELF.lpSmallVan;
  END GetLSGeneratorMask;

PROCEDURE GetLSWavelet0Mask (SELF: T; ): S.T =
  BEGIN
    RETURN SELF.hpSmallVan;
  END GetLSWavelet0Mask;

PROCEDURE GetShapeWaveletMask (SELF: T; READONLY mc: Parameters; ): S.T =
  (* Reverse translation that was made to center the masks. *)
  BEGIN
    RETURN SELF.hpSmallVan.superpose(
             SELF.lpSmallVan.upConvolve(
               mc.lift.scale(R.One / mc.wavelet0Amp), 2)).translate(
             -SELF.shiftSmallVan);
  END GetShapeWaveletMask;


PROCEDURE StdGetFilterBank (SELF: Standard; READONLY mc: Parameters; ):
  ARRAY [0 .. 1] OF FB.TBody =
  VAR dual := FB.TBody{SELF.getRefineMask(), SELF.getShapeWaveletMask(mc)};
  BEGIN
    RETURN ARRAY OF FB.TBody{FB.DualToPrimal(dual), dual};
  END StdGetFilterBank;

PROCEDURE StdPlotBase
  (SELF: Standard; READONLY mc: Parameters; numLevels: CARDINAL; ) =
  CONST
    yMin = -3.5D0;
    yMax = 3.5D0;
  BEGIN
    WPlot.BiorthogonalYLim(
      SELF.getRefineMask(), SELF.getShapeWaveletMask(mc), numLevels, yMin,
      yMax);
  END StdPlotBase;


PROCEDURE NoVanGetFilterBank
  (SELF: NonVanishing; READONLY mc: Parameters; ):
  ARRAY [0 .. 1] OF FB.TBody =
  BEGIN
    RETURN ARRAY [0 .. 1] OF
             FB.TBody{
             FB.DualToPrimal(
               FB.TBody{
                 SELF.lp, SELF.hp.superpose(
                            SELF.lpVan.upConvolve(
                              mc.lift.scale(R.One / mc.wavelet0Amp), 2))}),
             FB.TBody{SELF.lp, SELF.getShapeWaveletMask(mc)}};
    (*
        RETURN ARRAY OF
                 FB.TBody{FB.DualToPrimal(
                        FB.TBody{SELF.lp,
                             SELF.hp.superpose(
                               SELF.lpVan.upConvolve(
                                 mc.lift.scale(R.One / mc.wavelet0Amp), 2))}),
                      FB.TBody{SELF.lp, SELF.getShapeWaveletMask(mc)}};
    *)
    (*
      Check if vanishing moments are added correctly.
      Note that the Dual filters are used for Analysis!

          vanBank := ARRAY [0 .. 1] OF
                       FB.TBody{FB.TBody{bank[0, 0], bank[0, 1]},
                            FB.TBody{bank[1, 0], SIntPow.MulPower(
                                               bank[1, 1], vanishingAtom, van1)}};
          VAR primal := FB.DualToPrimal(vanBank[1]);
          BEGIN
            IO.Put(
              Fmt.FN("Filter bank should be:\n%s\n%s\n%s\n%s\n",
                     ARRAY OF
                       TEXT{SF.Fmt(vanBank[0, 0]), SF.Fmt(primal[0]),
                            SF.Fmt(vanBank[0, 1]), SF.Fmt(primal[1])}));
          END;
    *)
  END NoVanGetFilterBank;

PROCEDURE NoVanPlotBase
  (SELF: NonVanishing; READONLY mc: Parameters; numLevels: CARDINAL; ) =
  CONST
    yMin = -8.0D0;
    yMax = 8.0D0;
  VAR
    bank       := SELF.getFilterBank(mc);
    refnPrimal := bank[0, 0];
    refnDual   := bank[1, 0];
  BEGIN
    WPlot.AnyYLim(
      refnPrimal.scale(R.Two), refnDual.scale(R.Two), bank[0, 0],
      bank[0, 1], bank[1, 0], bank[1, 1], numLevels, yMin, yMax);
    (*
    WPlot.Any(refnPrimal.scale(R.Two), refnDual.scale(R.Two), bank[0, 0],
                bank[0, 1], bank[1, 0], bank[1, 1], numLevels);
    *)
  END NoVanPlotBase;


PROCEDURE InitBSpline
  (basis: T; dualSmooth, primalSmooth, dualVanishing: CARDINAL; ): T =

  <* FATAL Arithmetic.Error *>   (* MulPower can't fail here. *)
  BEGIN
    <* ASSERT primalSmooth >= dualVanishing,
                "It must be primalSmooth >= dualVanishing" *>

    basis.shiftVan := 2 - dualSmooth - primalSmooth;
    basis.shiftSmallVan := (primalSmooth - dualVanishing) DIV 2 - 1;

    basis.lp := BSpline.GeneratorMask(dualSmooth);
    basis.hp := BSpline.WaveletMask(dualSmooth, primalSmooth);

    basis.lpVan :=
      SIntPow.MulPower(basis.lp, vanishingAtom2, primalSmooth).translate(
        basis.shiftVan);
    (* GeneratorMask uses factors (0.5,0.5), Vanishing moments (0.5,-0.5),
       this results in (0.25,0,-0.25) but the lifting mask is convolved with
       (0.5,-0.5).  We have to compensate that with 'scale'. *)
    (* Translate mask so that it is symmetric again. *)
    basis.lpSmallVan :=
      SIntPow.MulPower(
        BSpline.GeneratorMask(dualSmooth + primalSmooth - dualVanishing),
        vanishingAtom2, dualVanishing).translate(
        basis.shiftVan + basis.shiftSmallVan).scale(
        RIntPow.MulPower(R.One, R.Two, primalSmooth - dualVanishing));
    basis.hpSmallVan :=
      BSpline.WaveletMask(dualSmooth + primalSmooth - dualVanishing,
                          dualVanishing).translate(basis.shiftSmallVan);

    (* compensate factors (0.5,0.5) from the mask *)
    basis.lpNoVan := BSpline.GeneratorMask(
                       dualSmooth + primalSmooth).translate(
                       basis.shiftVan).scale(
                       RIntPow.MulPower(R.One, R.Two, primalSmooth));
    basis.hpNoVan := BSpline.WaveletMask(dualSmooth + primalSmooth, 0);

    RETURN basis;
  END InitBSpline;

BEGIN
  vanishingAtom := NEW(S.T).fromArray(ARRAY OF R.T{R.Half, -R.Half});
  vanishingAtom2 :=
    NEW(S.T).fromArray(ARRAY OF R.T{R.Half, R.Zero, -R.Half});
END WaveletMatchBasis.
