MODULE TestSmoothness;

IMPORT LongRealBasic              AS R,
       LongRealComplexTrans       AS CT,
       LongRealVector             AS V,
       LongRealVectorFast         AS VFs,
       LongRealVectorTrans        AS VT,
       LongRealComplexVector      AS CV,
       LongRealComplexVectorTrans AS CVT,
       LongRealSignal             AS S,
       LongRealBSplineWavelet     AS BSpl,
       LongRealSignalFmtLex       AS SF,
       LongRealRefinableFunc      AS Refn,
       LongRealWaveletPlot        AS WP,
       LongRealFFTWRaw            AS FFT,
       Integer32IntegerPower      AS IIntPow,
       PLPlot                     AS PL;

IMPORT IO, Fmt, Wr, Thread;
(*IMPORT NADefinitions AS NA;*)

PROCEDURE DFTR2C1D (READONLY x: ARRAY OF R.T;
  (*flags := FFT.FlagSet{FFT.Flag.Estimate};*)): CV.T =
  VAR
    z    := CV.New(NUMBER(x) DIV 2 + 1);
    plan := FFT.PlanDFTR2C1D(NUMBER(x), x[0], z[0], 2_1000000);
  BEGIN
    TRY FFT.Execute(plan); FINALLY FFT.DestroyPlan(plan); END;
    RETURN z;
  END DFTR2C1D;

PROCEDURE SpecNorm (x: CV.T; rsize: CARDINAL; ): R.T =
  BEGIN
    IF rsize MOD 2 = 0 THEN
      RETURN
        R.Two * CVT.Norm2Sqr(x) - CT.AbsSqr(x[0]) - CT.AbsSqr(x[LAST(x^)]);
    ELSE
      RETURN R.Two * CVT.Norm2Sqr(x) - CT.AbsSqr(x[0]);
    END;
  END SpecNorm;

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
          VAR
            padded  := xn[l].clipToVector(0, xn[l].getNumber() * 2);
            spec    := DFTR2C1D(padded^);
            absspec := V.New(NUMBER(spec^));
          BEGIN
            FOR k := FIRST(absspec^) TO LAST(absspec^) DO
              absspec[k] := CT.Abs(spec[k]);
            END;
            IO.Put(
              Fmt.FN(
                "  Euclidean norm %s (spec %s), Sum norm %s, support [%s,%s]\n",
                ARRAY OF
                  TEXT{Fmt.LongReal(VT.Norm2Sqr(xn[l].getData()) * FLOAT(
                                      NUMBER(padded^), R.T)),
                       Fmt.LongReal(SpecNorm(spec, NUMBER(padded^))),
                       Fmt.LongReal(VT.Norm1(xn[l].getData())),
                       Fmt.Int(xn[l].getFirst()), Fmt.Int(xn[l].getLast())}));

            (*
            VAR
              unit  := IIntPow.MulPower(1, 2, l);
              grid  := R.One / FLOAT(unit, R.T);
              left  := FLOAT(FIRST(absspec^), R.T) * grid;
              right := FLOAT(LAST(absspec^), R.T) * grid;
            <* FATAL PL.SizeMismatch *> (*Number of filters and channels
                                           will always match*)
            BEGIN
              PL.Init();
              PL.SetFGColorDiscr(1);
              PL.SetEnvironment(
                left, right, VFs.Min(absspec^), VFs.Max(absspec^));
              PL.SetFGColorDiscr(2);
              PL.PlotLines(
                V.ArithSeq(NUMBER(absspec^), R.Zero * grid, grid)^,
                absspec^);
              PL.Exit();
            END;
            *)
          END;
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
