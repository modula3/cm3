MODULE TestSmoothness;

IMPORT LongRealBasic                AS R,
       LongRealComplexTrans         AS CT,
       LongRealVector               AS V,
       LongRealVectorFast           AS VFs,
       LongRealVectorTrans          AS VT,
       LongRealComplexVector        AS CV,
       LongRealComplexVectorSupport AS CVS,
       LongRealComplexVectorTrans   AS CVT,
       LongRealSignal               AS S,
       LongRealComplexSignal        AS CS,
       LongRealBSplineWavelet       AS BSpl,
       LongRealFmtLex               AS RF,
       LongRealSignalFmtLex         AS SF,
       LongRealRefinableFunc        AS Refn,
       LongRealWaveletPlot          AS WP,
       LongRealFFTWRaw              AS FFT,
       Integer32IntegerPower        AS IIntPow,
       PLPlot                       AS PL;

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

PROCEDURE PlotReal (s: S.T; l: CARDINAL; ) =
  VAR
    unit  := IIntPow.MulPower(1, 2, l);
    grid  := R.One / FLOAT(unit, R.T);
    left  := FLOAT(s.getFirst(), R.T) * grid;
    right := FLOAT(s.getLast(), R.T) * grid;
  <* FATAL PL.SizeMismatch *>
  BEGIN
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      left, right, VFs.Min(s.getData()^), VFs.Max(s.getData()^));
    PL.SetFGColorDiscr(2);
    PL.PlotLines(
      V.ArithSeq(s.getNumber(), FLOAT(s.getFirst(), R.T) * grid, grid)^,
      s.getData()^);
  END PlotReal;

PROCEDURE PlotComplex (s: CS.T; l: CARDINAL; ) =
  VAR
    unit     := IIntPow.MulPower(1, 2, l);
    grid     := R.One / FLOAT(unit, R.T);
    left     := FLOAT(s.getFirst(), R.T) * grid;
    right    := FLOAT(s.getLast(), R.T) * grid;
    vre, vim := NEW(V.T, s.getNumber());

  <* FATAL PL.SizeMismatch *>
  BEGIN
    WITH v = s.getData()^ DO
      FOR i := FIRST(vre^) TO LAST(vre^) DO
        vre[i] := v[i].re;
        vim[i] := v[i].im;
      END;
    END;
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(left, right, MIN(VFs.Min(vre^), VFs.Min(vim^)),
                      MAX(VFs.Max(vre^), VFs.Max(vim^)));
    WITH abscissa = V.ArithSeq(s.getNumber(),
                               FLOAT(s.getFirst(), R.T) * grid, grid)^ DO
      PL.SetFGColorDiscr(2);
      PL.PlotLines(abscissa, vre^);
      PL.SetFGColorDiscr(3);
      PL.PlotLines(abscissa, vim^);
    END;
  END PlotComplex;

(* Test FFT, check equivalence of Euclidean norm in the signal space and in
   the frequency space. *)
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

            (*PlotReal(absspec, l);*)
          END;
        END;
      END;
    END;
  END BSplineSmoothness;


PROCEDURE FourierDecay () =
  VAR
    (* A mask close to the one of the hat function. *)
    mask := NEW(S.T).fromArray(
              ARRAY OF R.T{0.23D0, 0.54D0, 0.23D0}, -1).autocorrelate();
    (* mask := NEW(S.T).fromArray(ARRAY OF R.T{0.23D0, 0.54D0, 0.23D0},
       -1);*)
    generator           := mask;
    twopow   : CARDINAL := 1;
  BEGIN
    FOR l := 0 TO 10 DO
      IO.Put(Fmt.FN("number of levels: %s\n", ARRAY OF TEXT{Fmt.Int(l)}));
      PlotReal(generator, l);
      VAR
        minsize := generator.getNumber() * 2;
        (*round up to the next multiple of twopow*)
        newsize   := minsize + (-minsize) MOD twopow;
        bandwidth := newsize DIV twopow;
        spec      := DFTR2C1D(generator.wrapCyclic(newsize)^);
      BEGIN
        PlotComplex(NEW(CS.T).fromVector(spec), l);
        (* Too high frequencies are equivalent to negative frequencies and
           must not be considered here.  They also don't appear in the
           spectrum as generated by FFTW. *)
        FOR k := 2 TO l DO
          WITH sum = CVS.Sum(SUBARRAY(spec^, bandwidth, bandwidth)) DO
            IO.Put(
              Fmt.FN("%s: %s\n", ARRAY OF TEXT{Fmt.Int(k), RF.Fmt(sum.re)}));
          END;
          bandwidth := bandwidth * 2;
        END;
      END;
      generator := Refn.Refine(generator, mask);
      twopow := twopow * 2;
    END;
  END FourierDecay;


PROCEDURE Test () =
  BEGIN
    PL.Init();
    CASE 1 OF
    | 0 => BSplineSmoothness();
    | 1 => FourierDecay();
    ELSE
      <* ASSERT FALSE *>
    END;
    PL.Exit();
  END Test;

BEGIN
END TestSmoothness.
