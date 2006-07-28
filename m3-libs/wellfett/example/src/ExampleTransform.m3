MODULE ExampleTransform;

IMPORT Arithmetic, Range;

IMPORT LongRealBasic              AS R,
       LongRealVector             AS V,
       LongRealSignal             AS S,
       LongRealSignalIntegerPower AS SIntPow,
       LongRealSignalFmtLex       AS SF;

IMPORT LongRealBSplineWavelet                 AS BSpline,
       LongRealDyadicDiscreteWaveletTransform AS DWT,
       LongRealDyadicFilterBank               AS FilterBank,
       LongRealWaveletMatchBasis              AS WMBasis,
       LongRealWaveletMatchSmooth             AS WaveletMatchSmooth;


IMPORT LongRealPLPlot AS PL, LongRealDWTPlot AS DWTPlot;
IMPORT IO, Fmt, Wr, Thread;


PROCEDURE PlotSWT
  (         x        : S.T;
   READONLY bank     : ARRAY [0 .. 1] OF FilterBank.TBody;
            numLevels: CARDINAL;                           ) =
  (* role of primal and dual filters is swapped here *)
  VAR
    wt    := DWT.ShiftInvariantFromSignal(x, bank[1], numLevels);
    recon := DWT.ShiftInvariantToSignal(wt, bank[0]);
  BEGIN
    PL.Init();
    DWTPlot.ShiftInvariantDWT(wt, recon);
    PL.Exit();
  END PlotSWT;

PROCEDURE PlotDWT
  (         x        : S.T;
   READONLY bank     : ARRAY [0 .. 1] OF FilterBank.TBody;
            vanishing: S.T;
            numLevels: CARDINAL;                           ) =

  VAR
    (* role of primal and dual filters is swapped here *)
    wt := DWT.HighBandOversampledFromSignal(x, bank[1], numLevels);
    recon := DWT.ToSignal(DWT.HighBandConvolveDown(wt, vanishing), bank[0]);

  BEGIN
    PL.Init();
    DWTPlot.DWT(wt, recon);
    PL.Exit();
  END PlotDWT;


PROCEDURE Run () =
  BEGIN
    CASE 1 OF
    | 0 =>
        VAR
          dual := FilterBank.TBody{
                    BSpline.GeneratorMask(2), BSpline.WaveletMask(2, 2)};
          primal := FilterBank.DualToPrimal(
                      ARRAY OF
                        S.T{dual[0].scale(R.Two), dual[1].scale(R.Two)});
        BEGIN
          PlotSWT(S.One, ARRAY OF FilterBank.TBody{primal, dual}, 6);
        END;
    | 1 =>
        CONST
          numLevels = 6;
          unit      = 64;
          size      = 8 * unit;
          smooth    = 3;
          van0      = 1;
          van1      = 6;

        <* FATAL Arithmetic.Error *> (* hope on convergence *)
        VAR
          bank := WaveletMatchSmooth.BSplineWithPattern(
                    NEW(S.T).fromArray(
                      V.ArithSeq(size, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
                      -size DIV 2), smooth, van0 + van1, van0,
                    Range.New(-7, 14), numLevels, 1.0D-10);
          (* Note that the dual filters are used for Analysis! *)
          reconBank := ARRAY [0 .. 1] OF
                         FilterBank.TBody{bank[0],
                                          FilterBank.TBody{
                                            bank[1, 0].scale(R.Two),
                                            bank[1, 1].scale(R.Two)}};
          vanishing := SIntPow.MulPower(S.One, WMBasis.vanishingAtom, van1);

        <* FATAL Thread.Alerted, Wr.Failure *>
        BEGIN
          IO.Put(
            Fmt.FN(
              "Reconstruction:\n%s\n%s\n",
              ARRAY OF
                TEXT{SF.Fmt(reconBank[0, 0].convolve(reconBank[1, 0])),
                     SF.Fmt(reconBank[0, 1].convolve(
                              reconBank[1, 1]).convolve(vanishing))}));
          PlotDWT(S.One, reconBank, vanishing, 4);
        END;
    ELSE
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END ExampleTransform.
