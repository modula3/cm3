MODULE TestBSpline;

IMPORT LongRealBasic AS R;
IMPORT LongRealFmtLex AS RF;
IMPORT LongRealTrans AS RT;
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
  <*FATAL BSpl.DifferentParity*>
  VAR
    prod0 := gdual.alternate().convolve(hdual);
    prod1 := hdual.alternate().convolve(gdual);
  BEGIN
    RETURN prod0.superpose(prod1.negate());
  END Reconstruction;

PROCEDURE CheckPerfectReconstruction () =
  <*FATAL BSpl.DifferentParity, Thread.Alerted, Wr.Failure*>
  VAR hdual, gdual: S.T;
  BEGIN
    FOR i := 0 TO 5 DO
      FOR j := i MOD 2 TO 5 BY 2 DO
        IO.Put(
          Fmt.FN("CDF-%s,%s: %s\n",
                 ARRAY OF
                   TEXT{Fmt.Int(i), Fmt.Int(j),
                        SF.Fmt(Reconstruction(BSpl.GeneratorMask(i),
                                              BSpl.WaveletMask(i, j)))}));
      END;
    END;
  END CheckPerfectReconstruction;

PROCEDURE Test () =
  BEGIN
    CASE 1 OF
    | 0 => ShowFilters();
    | 1 => CheckPerfectReconstruction();
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestBSpline.
