MODULE TestBSpline;

(*IMPORT LongRealBasic AS R;*)
IMPORT LongRealSignal AS S;

IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT LongRealPLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
(*IMPORT Arithmetic AS Arith;*)

PROCEDURE ShowFilters () =
  <* FATAL Thread.Alerted, Wr.Failure *>
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
  <* FATAL Thread.Alerted, Wr.Failure *>
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

PROCEDURE ShowBSplWavelets () =
  BEGIN
    PL.Init();
    WP.PlotBiorthogonal(BSpl.GeneratorMask(3), BSpl.WaveletMask(3, 9), 6);
    PL.Exit();
  END ShowBSplWavelets;

PROCEDURE Test () =
  BEGIN
    CASE 2 OF
    | 0 => ShowFilters();
    | 1 => CheckPerfectReconstruction();
    | 2 => ShowBSplWavelets();
    ELSE
      <* ASSERT FALSE *>
    END;
  END Test;

BEGIN
END TestBSpline.
