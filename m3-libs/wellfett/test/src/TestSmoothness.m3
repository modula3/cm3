MODULE TestSmoothness;

IMPORT                           (* LongRealBasic AS R,*)
  LongRealSignal         AS S,
  LongRealBSplineWavelet AS BSpl,
  LongRealSignalFmtLex   AS SF,
  LongRealRefinableFunc  AS Refn,
  LongRealWaveletPlot    AS WP,
  LongRealFFTW           AS FFT,
  LongRealVectorTrans    AS VT,
  PLPlot                 AS PL;

IMPORT IO, Fmt, Wr, Thread;
(*IMPORT NADefinitions AS NA;*)

PROCEDURE BSplineSmoothness () =
  VAR x: ARRAY [1 .. 7], [0 .. 6] OF S.T;
  BEGIN
    FOR n := FIRST(x) TO LAST(x) DO
      WITH mask = BSpl.GeneratorMask(n),
           xn   = x[n]                   DO
        xn[0] := mask;
        FOR l := 1 TO LAST(xn) DO
          xn[l] := Refn.Refine(xn[l - 1], mask);
        END;
        IO.Put(Fmt.FN("spline order %s\n", ARRAY OF TEXT{Fmt.Int(n)}));
        FOR l := FIRST(xn) TO LAST(xn) DO
          IO.Put(
            Fmt.FN(
              "  Euclidean norm %s, Sum norm %s, support [%s,%s]\n",
              ARRAY OF
                TEXT{Fmt.LongReal(VT.Norm2Sqr(xn[l].getData())),
                     Fmt.LongReal(VT.Norm1(xn[l].getData())),
                     Fmt.Int(xn[l].getFirst()), Fmt.Int(xn[l].getLast())}));
        END;
      END;
    END;
  END BSplineSmoothness;

PROCEDURE Test () =
  BEGIN
    CASE 0 OF | 0 => BSplineSmoothness(); ELSE <* ASSERT FALSE *> END;
  END Test;

BEGIN
END TestSmoothness.
