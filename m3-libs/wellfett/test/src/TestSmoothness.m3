MODULE TestSmoothness;

IMPORT LongRealBasic                AS R,
       LongRealTrans                AS RT,
       LongRealComplex              AS C,
       LongRealComplexTrans         AS CT,
       LongRealVector               AS V,
       LongRealVectorFast           AS VFs,
       LongRealVectorTrans          AS VT,
       LongRealComplexVector        AS CV,
       LongRealComplexVectorBasic   AS CVB,
       LongRealComplexVectorSupport AS CVS,
       LongRealComplexVectorTrans   AS CVT,
       LongRealMatrix               AS M,
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
(*IMPORT Arithmetic AS Arith;*)

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
  CONST magnify = 1.0D0;

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
    PL.SetEnvironment(
      left, right, MIN(VFs.Min(vre^), VFs.Min(vim^)) / magnify,
      MAX(VFs.Max(vre^), VFs.Max(vim^)) / magnify);
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



PROCEDURE MulVecC (VAR (*OUT*) z   : ARRAY OF C.T;
                   READONLY    x, y: ARRAY OF C.T; ) =
  BEGIN
    <* ASSERT 2 * (NUMBER(x) - 1) = NUMBER(y) *>
    <* ASSERT NUMBER(y) = NUMBER(z) *>
    FOR i := FIRST(x) TO LAST(x) DO z[i] := C.Mul(x[i], y[i]); END;
    FOR i := FIRST(x) + 1 TO LAST(x) - 1 DO
      z[NUMBER(z) - i] := C.Mul(C.Conj(x[i]), y[NUMBER(z) - i]);
    END;
  END MulVecC;

PROCEDURE UpSample2 (READONLY x: ARRAY OF C.T; ): REF ARRAY OF C.T =
  VAR z := NEW(CV.T, 2 * NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) - 1 DO
      z[2 * i] := x[i];
      z[2 * i + 1] := C.Scale(C.Add(x[i], x[i + 1]), R.Half);
    END;
    z[LAST(z^) - 1] := x[LAST(x)];
    z[LAST(z^)] := C.Scale(C.Add(x[LAST(x)], x[FIRST(x)]), R.Half);
    RETURN z;
  END UpSample2;

PROCEDURE FourierDecay () =
  VAR
    (* A mask close to the one of the hat function. *)
    mask0 := NEW(S.T).fromArray(
               ARRAY OF R.T{0.25D0, 0.5D0, 0.25D0}, -1).autocorrelate();
    mask1 := NEW(S.T).fromArray(
               ARRAY OF R.T{0.26D0, 0.5D0, 0.24D0}, -1).autocorrelate();
    mask2 := NEW(S.T).fromArray(
               ARRAY OF R.T{0.3D0, 0.5D0, 0.2D0}, -1).autocorrelate();
    mask3 := NEW(S.T).fromArray(
               ARRAY OF R.T{0.23D0, 0.54D0, 0.23D0}, -1).autocorrelate();
    mask := NEW(S.T).fromArray(ARRAY OF R.T{0.2D0, 0.8D0}).autocorrelate();
    mask5 := NEW(S.T).fromArray(ARRAY OF R.T{0.23D0, 0.54D0, 0.23D0}, -1);
    generator           := mask;
    twopow   : CARDINAL := 1;

  BEGIN
    FOR l := 0 TO 12 DO
      IO.Put(Fmt.FN("number of levels: %s\n", ARRAY OF TEXT{Fmt.Int(l)}));
      PlotReal(generator, l);
      VAR
        minsize := generator.getNumber() * 2;
        (*round up to the next multiple of twopow*)
        newsize   := minsize + (-minsize) MOD twopow;
        bandwidth := newsize DIV twopow;
        genSpec   := DFTR2C1D(generator.wrapCyclic(newsize)^);

        (*quite a random start filter*)
        refnMask := NEW(S.T).fromArray(
                      ARRAY OF R.T{-0.3D0, 1.85D0, -0.55D0}, -1);

      BEGIN

        IF l > 1 THEN
          VAR
            rep     : CARDINAL := 2;
            maskSpec           := DFTR2C1D(mask.wrapCyclic(bandwidth)^);
            band := CV.FromArray(SUBARRAY(genSpec^, bandwidth, bandwidth));
            decaySpec           := CV.New(NUMBER(genSpec^));
            k        : CARDINAL := l - 1;
          BEGIN
            (*CVS.Clear(SUBARRAY(decaySpec^, 0, bandwidth));*)
            SUBARRAY(decaySpec^, 0, bandwidth) :=
              SUBARRAY(genSpec^, 0, bandwidth);
            LOOP
              SUBARRAY(decaySpec^, NUMBER(band^), NUMBER(band^)) := band^;
              WITH genBand = SUBARRAY(
                               genSpec^, NUMBER(band^), NUMBER(band^)),
                   sum  = CVS.Sum(genBand).re,
                   bias = CVS.Sum(band^).re    DO
                IO.Put(
                  Fmt.FN(
                    "diff %s: %s, sum (%s) %s <-> %s\n",
                    ARRAY OF
                      TEXT{Fmt.Int(k),
                           RF.Fmt(CVT.Norm2(CVB.Sub(band^, genBand))),
                           RF.Fmt(sum / bias), RF.Fmt(sum), RF.Fmt(bias)}));
                DEC(k);
              END;
              IF k = 0 THEN EXIT END;
              band := UpSample2(band^);
              FOR i := 0 TO rep - 1 DO
                WITH bandSpec = SUBARRAY(band^, bandwidth * i, bandwidth) DO
                  MulVecC(bandSpec, maskSpec^, bandSpec);
                END;
              END;
              rep := rep * 2;
            END;
            PlotComplex(NEW(CS.T).fromVector(decaySpec), l);
          END;
        END;

        PlotComplex(NEW(CS.T).fromVector(genSpec), l);

        (* High frequencies are equivalent to negative frequencies and must
           not be considered here.  They also don't appear in the spectrum
           as generated by FFTW. *)
        FOR k := 2 TO l DO
          WITH sum  = CVS.Sum(SUBARRAY(genSpec^, bandwidth, bandwidth)).re,
               bias = refnMask.getValue(0)                                  DO
            IO.Put(Fmt.FN("%s: (%s) %s <-> %s\n",
                          ARRAY OF
                            TEXT{Fmt.Int(k), RF.Fmt(sum / bias),
                                 RF.Fmt(sum), RF.Fmt(bias)}));
          END;
          refnMask := refnMask.convolveDown(mask, 2);
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
