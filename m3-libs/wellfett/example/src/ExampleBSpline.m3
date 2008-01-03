MODULE ExampleBSpline;

IMPORT LongRealSignal AS S;

IMPORT LongRealBSplineWavelet AS BSpline, LongRealDyadicFilterBank AS FB;

IMPORT LongRealSignalFmtLex AS SF,
       LongRealWaveletPlot  AS WPlot,
       LongRealPLPlot       AS PL;
IMPORT IO, Fmt, Wr, Thread;


PROCEDURE ShowFilters () =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    IO.Put(Fmt.FN("dual generator: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpline.GeneratorMask(2))}));
    IO.Put(Fmt.FN("dual wavelet: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpline.WaveletMask(2, 2))}));
  END ShowFilters;

PROCEDURE Reconstruction (lpDual, hpDual: S.T): S.T =
  VAR
    primal := FB.DualToPrimal(FB.TBody{lpDual, hpDual});
    prod0  := primal[0].convolve(lpDual);
    prod1  := primal[1].convolve(hpDual);
  BEGIN
    RETURN prod0.superpose(prod1);
  END Reconstruction;

PROCEDURE CheckPerfectReconstruction () =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    FOR i := 0 TO 6 DO
      FOR j := i MOD 2 TO 8 BY 2 DO
        IO.Put(
          Fmt.FN("CDF-%s,%s: %s\n",
                 ARRAY OF
                   TEXT{Fmt.Int(i), Fmt.Int(j),
                        SF.Fmt(Reconstruction(BSpline.GeneratorMask(i),
                                              BSpline.WaveletMask(i, j)))}));
      END;
    END;
  END CheckPerfectReconstruction;

PROCEDURE ShowBSplWavelets () =
  BEGIN
    PL.Init();
    WPlot.Biorthogonal(BSpline.GeneratorMask(3), BSpline.WaveletMask(3, 9), 6);
    PL.Exit();
  END ShowBSplWavelets;

PROCEDURE Run () =
  BEGIN
    CASE 1 OF
    | 0 => ShowFilters();
    | 1 => CheckPerfectReconstruction();
    | 2 => ShowBSplWavelets();
    ELSE
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END ExampleBSpline.
