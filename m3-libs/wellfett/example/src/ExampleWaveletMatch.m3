MODULE ExampleWaveletMatch;

IMPORT LongRealBasic         AS R,
       LongRealTrans         AS RT,
       Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector      AS V,
       LongRealVectorFast  AS VFs,
       LongRealVectorTrans AS VT;

IMPORT LongRealMatrix AS M;

IMPORT LongRealWaveletMatchGradient AS WMGrad,
       LongRealWaveletMatchBasis    AS WMBasis,
       LongRealWaveletMatch         AS WaveletMatch,
       LongRealWaveletMatchSmooth   AS WaveletMatchSmooth;

IMPORT LongRealSignal AS S, LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunction AS Refn,
       LongRealBSplineWavelet    AS BSpline,
       LongRealDyadicFilterBank  AS FB;

IMPORT LongRealFmtLex       AS RF,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixFmtLex AS MF,
       LongRealSignalFmtLex AS SF,
       LongRealWaveletPlot  AS WPlot;
IMPORT LongRealPLPlot AS PL;
IMPORT IO, Fmt, FileRd, Wr, TextWr, Thread;

IMPORT OSError, FloatMode, Lex, Rd;

IMPORT Range;
IMPORT Arithmetic AS Arith;


(* {R.Half, R.Zero, -R.Half} (WMBasis.vanishingAtom2) instead of {R.One,
   R.Zero, -R.One} has the advantage that the sum of the coefficients of
   the primal filter doesn't change. *)

PROCEDURE TestMatchPattern (target                  : S.T;
                            smooth, vanishing       : CARDINAL;
                            numLevels, numTranslates: CARDINAL; ) =
  (* The degree of freedom, i.e.  the number of parameters to minimize for,
     is 2*numTranslates *)
  <* FATAL Arith.Error *>        (* MulPower can't fail for signals *)
  VAR
    translates := Range.New(-numTranslates, 2 * numTranslates);
    lpDual     := BSpline.GeneratorMask(smooth).scale(R.Two).translate(2);
    hpDual     := BSpline.WaveletMask(smooth, vanishing).scale(R.Two);
    lpDualVan := SIntPow.MulPower(
                   lpDual, WMBasis.vanishingAtom2, vanishing).translate(
                   -smooth - vanishing);

    (* Efficiency note: WaveletMatch.WithPattern also calls
       WaveletMatch.ComputeLeastSquaresProblem *)
    lsqr := WaveletMatch.ComputeLeastSquaresProblem(
              target, lpDual, lpDualVan, hpDual, translates, numLevels);
    approx := WaveletMatch.WithPattern(
                target, lpDual, lpDualVan, hpDual, translates, numLevels);

    altWavelet0Amp := WaveletMatch.WithPatternWavelet(
                        target, lpDual, hpDual, numLevels);
    targetSubWavelet0 := target.superpose(
                           Refn.Refine(hpDual.scale(-altWavelet0Amp),
                                       lpDual, numLevels));
    altApprox := WaveletMatch.WithPatternGenerator(
                   targetSubWavelet0, lpDual, lpDualVan, translates,
                   numLevels);
    altWaveletMask := hpDual.scale(altWavelet0Amp).superpose(
                        lpDualVan.upConvolve(altApprox.lift, 2));

    unit   := IIntPow.MulPower(1, 2, numLevels);
    twoPow := FLOAT(unit, R.T);
    grid   := R.One / twoPow;

    first    := approx.approx.getFirst();
    size     := approx.approx.getNumber();
    abscissa := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    IO.Put(Fmt.FN("%s - %s\n", ARRAY OF
                                 TEXT{RF.Fmt(approx.wavelet0Amp),
                                      RF.Fmt(altWavelet0Amp)}));
    IO.Put(
      Fmt.FN("lift:\n%s\n%s\n",
             ARRAY OF TEXT{SF.Fmt(approx.lift), SF.Fmt(altApprox.lift)}));
    PL.Init();
    WPlot.Frame(abscissa^, SUBARRAY(lsqr.basis^, 0, 2 * numTranslates),
                lsqr.basis[LAST(lsqr.basis^)], lsqr.targetPad^);
    PL.SetFGColorDiscr(4);
    PL.PlotLines(abscissa^, approx.approx.getData()^);
    PL.SetFGColorDiscr(5);
    PL.PlotLines(
      abscissa^,
      Refn.Refine(altWaveletMask, lpDual, numLevels).clipToVector(
        first, size)^);

    IF FALSE THEN
      PL.SetFGColorDiscr(6);
      PL.PlotLines(abscissa^, targetSubWavelet0.clipToVector(first, size)^);
    END;
    PL.Exit();
  END TestMatchPattern;


PROCEDURE TestMatchPatternSmooth
  (target                                 : S.T;
   dualSmooth, primalSmooth, dualVanishing: CARDINAL;
   numLevels, numTranslates               : CARDINAL;
   smoothWeight                           : R.T;      ):
  ARRAY [0 .. 1] OF FB.TBody =
  <* FATAL Arith.Error, Thread.Alerted, Wr.Failure *>

  PROCEDURE WaveletMatchSmoothWithPattern (): WMGrad.Parameters =
    VAR mc: WMGrad.Parameters;
    BEGIN
      PL.Init();
      mc := WaveletMatchSmooth.WithPattern(
              target, dualBasis, translates, numLevels, smoothWeight,
              WaveletMatchSmooth.FlagSet{WaveletMatchSmooth.Flag.Verbose,
                                         WaveletMatchSmooth.Flag.Plot});
      PL.Exit();
      RETURN mc;
    END WaveletMatchSmoothWithPattern;


  VAR
    translates := Range.New(-numTranslates, 2 * numTranslates);
    dualBasis := WMBasis.InitBSpline(NEW(WMBasis.NonVanishing), dualSmooth,
                                     primalSmooth, dualVanishing);
    (*
    mc := WMGrad.Parameters{NEW(S.T).fromArray(
                      ARRAY OF R.T{1.0D0, 0.0D0, 0.0D0, 1.0D0}), 1.5D0};
    *)
    mc := WaveletMatchSmoothWithPattern();
    s := SIntPow.MulPower(mc.lift.translate(dualBasis.shiftVan DIV 2),
                          WMBasis.vanishingAtom, primalSmooth);
    hpA       := dualBasis.hp.scale(mc.wavelet0Amp);
    hpLiftedA := hpA.superpose(dualBasis.lp.upConvolve(s, 2));
    hpLifted := dualBasis.hp.superpose(
                  dualBasis.lp.scale(R.One / mc.wavelet0Amp).upConvolve(
                    s, 2));
    unit              := IIntPow.Power(2, numLevels);
    twoPow            := FLOAT(unit, R.T);
    grid              := R.One / twoPow;
    waveletDual0      := Refn.Refine(hpA, dualBasis.lp, numLevels);
    waveletDual       := Refn.Refine(hpLiftedA, dualBasis.lp, numLevels);
    leftWaveletDual0  := FLOAT(waveletDual0.getFirst(), R.T) * grid;
    rightWaveletDual0 := FLOAT(waveletDual0.getLast(), R.T) * grid;
    leftWaveletDual   := FLOAT(waveletDual.getFirst(), R.T) * grid;
    rightWaveletDual  := FLOAT(waveletDual.getLast(), R.T) * grid;
    leftTarget        := FLOAT(target.getFirst(), R.T) * grid;
    rightTarget       := FLOAT(target.getLast(), R.T) * grid;

  CONST
    yMin = -1.5D0;
    yMax = 1.5D0;

  BEGIN
    IO.Put(
      Fmt.FN(
        "orig with all vanishing moments:\nlp %s,\nhp %s\n"
          & "only some of the vanishing moments, extended to full vanishing:\n"
          & "lp %s,\nhp %s\n",
        ARRAY OF
          TEXT{SF.Fmt(dualBasis.lpVan), SF.Fmt(dualBasis.hp),
               SF.Fmt(SIntPow.MulPower(
                        dualBasis.lpSmallVan, WMBasis.vanishingAtom,
                        primalSmooth - dualVanishing).translate(
                        -dualBasis.shiftSmallVan)),
               SF.Fmt(SIntPow.MulPower(
                        dualBasis.hpSmallVan, WMBasis.vanishingAtom,
                        primalSmooth - dualVanishing).translate(
                        -dualBasis.shiftSmallVan))}));
    IO.Put(
      Fmt.FN(
        "optimal lift %s,\ncyclic wrap of hpLifted %s\n\n"
        (* & "hsDual\n%s\n%s\n\n" & "hpDual0\n%s\n%s\n\n" &
           "hpLifted\n%s\n%s\n" *),
        ARRAY OF
          TEXT{SF.Fmt(s), VF.Fmt(hpLifted.alternate().wrapCyclic(3))
          (*, SF.Fmt(dualBasis.lp.upConvolve(s,2)),
             SF.Fmt(SIntPow.MulPower(lpDualNoVan.upConvolve(mc.lift,2),
             WMBasis.vanishingAtom, primalSmooth)), SF.Fmt(hpDual0),
             SF.Fmt(SIntPow.MulPower(hpDual0NoVan, WMBasis.vanishingAtom,
             primalSmooth)), SF.Fmt(hpLifted), SF.Fmt(SIntPow.MulPower(
             GetLiftedPrimalGeneratorMask( lpDualNoVan, hpDual0NoVan,
             mc).alternate(), WMBasis.vanishingAtom, primalSmooth))*)}));
    CASE 0 OF
    | 0 =>
    | 1 =>
        PL.Init();
        WPlot.Biorthogonal(dualBasis.lp, hpLifted, numLevels);
        PL.Exit();
    | 2 =>
        PL.Init();
        PL.SetEnvironment(
          MIN(leftTarget, MIN(leftWaveletDual, leftWaveletDual0)),
          MAX(rightTarget, MAX(rightWaveletDual, rightWaveletDual0)), yMin,
          yMax);
        PL.SetFGColorDiscr(3);
        PL.PlotLines(V.ArithSeq(target.getNumber(), leftTarget, grid)^,
                     target.getData()^);
        PL.SetFGColorDiscr(1);
        PL.PlotLines(
          V.ArithSeq(waveletDual0.getNumber(), leftWaveletDual0, grid)^,
          waveletDual0.getData()^);
        PL.SetFGColorDiscr(4);
        PL.PlotLines(
          V.ArithSeq(waveletDual.getNumber(), leftWaveletDual, grid)^,
          waveletDual.getData()^);
        PL.Exit();
    ELSE
      <* ASSERT FALSE *>
    END;
    RETURN dualBasis.getFilterBank(mc);
  END TestMatchPatternSmooth;


(* create symmetric clip of the sin x / x curve *)
PROCEDURE SincVector (size, width: CARDINAL): V.T =
  VAR
    z := V.New(2 * size + 1);
    k := RT.Pi / FLOAT(2 * width, R.T);
  BEGIN
    z[size] := R.One;
    FOR i := 1 TO size - 1 DO
      WITH x = FLOAT(i, R.T) * k,
           y = RT.Sin(x) / x      DO
        z[size + i] := y;
        z[size - i] := y;
      END;
    END;
    RETURN z;
  END SincVector;

PROCEDURE GaussianVector (size, width: CARDINAL): V.T =
  VAR
    z := V.New(2 * size + 1);
    k := R.One / FLOAT(width, R.T);
  BEGIN
    z[size] := R.One;
    FOR i := 1 TO size - 1 DO
      WITH x = FLOAT(i, R.T) * k,
           y = RT.Exp(-x * x)     DO
        z[size + i] := y;
        z[size - i] := y;
      END;
    END;
    RETURN z;
  END GaussianVector;

(* a sinc function that is modulated such that it covers a band ready for
   dyadic partitioning *)
PROCEDURE ModulateReal (x: V.T; period: R.T): V.T =
  VAR
    size := (NUMBER(x^) - 1) DIV 2;
    z    := V.New(2 * size + 1);
    k    := RT.TwoPi / FLOAT(period, R.T);
  BEGIN
    z[size] := x[size];
    FOR i := 1 TO size - 1 DO
      WITH c = RT.Cos(FLOAT(i, R.T) * k) DO
        z[size + i] := x[size + i] * c;
        z[size - i] := x[size - i] * c;
      END;
    END;
    RETURN z;
  END ModulateReal;

PROCEDURE ModulateImag (x: V.T; period: R.T): V.T =
  VAR
    size := (NUMBER(x^) - 1) DIV 2;
    z    := V.New(2 * size + 1);
    k    := RT.TwoPi / FLOAT(period, R.T);
  BEGIN
    z[size] := R.Zero;
    FOR i := 1 TO size - 1 DO
      WITH c = RT.Sin(FLOAT(i, R.T) * k) DO
        z[size + i] := x[size + i] * c;
        z[size - i] := -x[size - i] * c;
      END;
    END;
    RETURN z;
  END ModulateImag;


(* multiplicate signal with ramp (linear progression) *)
PROCEDURE MulRamp (x: S.T; scale: R.T; ): S.T =
  VAR
    xData := x.getData();
    zData := NEW(V.T, x.getNumber());
    j     := 0;
  BEGIN
    FOR i := x.getFirst() TO x.getLast() DO
      zData[j] := xData[j] * FLOAT(i, R.T) * scale;
      INC(j);
    END;
    RETURN NEW(S.T).fromVector(zData, x.getFirst());
  END MulRamp;

(* ***** should be moved to test suite ***** *)
PROCEDURE CheckVanishingMoments () =
  CONST
    size  = 1024;
    width = 300;
    step  = 50;
    scale = R.One / FLOAT(size, R.T);
  VAR difSig: S.T;
  BEGIN
    CASE 2 OF
    | 0 =>
        difSig := NEW(S.T).fromVector(GaussianVector(size, width), -size);
    | 1 =>
        difSig := NEW(S.T).fromVector(GaussianVector(size, width), -size);
        difSig := difSig.translate(-size DIV 4).scale(0.9D0).superpose(
                    difSig.translate(size DIV 3).scale(0.3D0));
    | 2 =>
        difSig :=
          NEW(S.T).fromVector(V.NewUniform(size + 1, R.Half), -size DIV 2);
    ELSE
      <* ASSERT FALSE *>
    END;

    FOR i := 0 TO 20 DO
      (*
      VAR
        minX := FLOAT(difSig.getFirst(), R.T);
        maxX := FLOAT(difSig.getLast(), R.T);
      BEGIN
        PL.Init();
        PL.SetEnvironment(minX, maxX, -R.One, R.One);
        PL.PlotLines(V.ArithSeq(difSig.getNumber(), minX, R.One)^,
                     difSig.getData()^);
        PL.Exit();
      END;
      *)
      (* versions of the Sig multiplied with ... *)
      VAR
        polyDifSig      := difSig; (* ...  power function *)
        chebyDifSig     := difSig; (* ...  chebyshev function *)
        chebyDifSigPrev := MulRamp(difSig, scale);
      BEGIN
        FOR j := 0 TO i DO
          VAR
            polyMom := polyDifSig.sum() / VT.Norm1(polyDifSig.getData());
            chebyMom := chebyDifSig.sum() / VT.Norm1(chebyDifSig.getData());
          BEGIN
            IO.Put(
              Fmt.FN("%2s. moment: %20s, %20s\n",
                     ARRAY OF
                       TEXT{Fmt.Int(j), RF.Fmt(polyMom), RF.Fmt(chebyMom)}));
          END;
          (*
          VAR
            minX     := FLOAT(difSig.getFirst(), R.T);
            maxX     := FLOAT(difSig.getLast(), R.T);
            abscissa := V.ArithSeq(difSig.getNumber(), minX, R.One);
          BEGIN
            PL.Init();
            PL.SetEnvironment(minX, maxX, -R.One, R.One);
            PL.SetFGColorDiscr(2);
            PL.PlotLines(abscissa^, polyDifSig.getData()^);
            PL.SetFGColorDiscr(3);
            PL.PlotLines(abscissa^, chebyDifSig.getData()^);
            PL.Exit();
          END;
          *)

          polyDifSig := MulRamp(polyDifSig, scale);
          (* apply recursive construction of chebyshev polynomials *)
          VAR
            tmp := MulRamp(chebyDifSig, scale * R.Two).superpose(
                     chebyDifSigPrev.negate());
          BEGIN
            chebyDifSigPrev := chebyDifSig;
            chebyDifSig := tmp;
          END;
        END;
        IO.Put("\n");
      END;
      difSig :=
        difSig.translate(step).superpose(difSig.translate(-step).negate());
    END;
  END CheckVanishingMoments;

PROCEDURE FmtSignalHaskell (x         : S.T;
                            numColumns: [1 .. LAST(CARDINAL)] := 3;
                            width     : [1 .. LAST(CARDINAL)] := 23; ):
  TEXT RAISES {Thread.Alerted, Wr.Failure} =
  VAR wr := TextWr.New();
  BEGIN
    Wr.PutText(
      wr, "ShiftedSignal.Cons (" & Fmt.Int(x.getFirst()) & ") (\n");
    FOR i := x.getFirst() TO x.getLast() BY numColumns DO
      Wr.PutText(wr, "  ");
      FOR j := i TO MIN(i + numColumns - 1, x.getLast()) DO
        Wr.PutText(
          wr, Fmt.F(" %s :", Fmt.Pad(RF.Fmt(x.getValue(j)), width)));
      END;
      Wr.PutText(wr, "\n");
    END;
    Wr.PutText(wr, "  [])\n");
    RETURN TextWr.ToText(wr);
  END FmtSignalHaskell;


PROCEDURE Run () =
  CONST
    numLevel = 6;
    unit     = 64;
  TYPE
    Example =
      {matchBSpline, matchBSplineVan, matchBSplineWavelet, matchRamp,
       matchRampSmooth, matchSincSmooth, matchGaussian, matchLongRamp,
       matchMassPeak, phdThesis, checkVanishingMoments};
  BEGIN
    CASE Example.phdThesis OF
    | Example.matchBSpline =>
        TestMatchPattern(Refn.Refine(S.One,
                                     BSpline.GeneratorMask(4).scale(2.0D0),
                                     7).translate(-50), 4, 0, numLevel, 5);
    | Example.matchBSplineVan =>
        TestMatchPattern(
          Refn.Refine(S.One, BSpline.GeneratorMask(1), 7).translate(10), 4,
          2, numLevel, 5);
    | Example.matchRamp =>
        (* The figures given here previously were wrong because the
           generator was convolved with (1,0,-1) instead of (1,-1)
           numTranslates 5, size 1917, residuum 0.00340514677538585,
           V11{0.186869299925214, 0.269986933917237, 0.670508585560263,
           0.649776682132423, -0.175806649674353, -0.875993675942413,
           -0.856283049545732, -0.458477950438848, -0.31397715987086,
           -0.11516417311729, ...} 0.330691666379811 *)
        TestMatchPattern(NEW(S.T).fromArray(
                           V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                           3 * unit - 4 * unit), 3, 1, numLevel, 5);
    | Example.matchRampSmooth =>
        (*
          EVAL TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), 4, 2, numLevel, 5, 50.0D0);
          EVAL TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), 4, 4, numLevel, 5, 20.0D0);
        *)

        CONST size = 20 * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
                 NEW(S.T).fromArray(
                   V.ArithSeq(size + 1, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
                   -size DIV 2), 3, 7, 1, numLevel, 10, 1.0D-12);
        END;

      (*
      FOR scale := 3 TO 10 DO
        VAR size := 2 * scale * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
            NEW(S.T).fromArray(
              V.ArithSeq(size, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
              -size DIV 2), 3, 7, 1, numLevel, 10, 1.0D-10);
        END;
      END;
      *)

    | Example.matchBSplineWavelet =>
        (*
          TestMatchPattern(
            Refn.Refine(
              BSpline.WaveletMask(2, 8), BSpline.GeneratorMask(2), numLevel).scale(
            FLOAT(unit,R.T)  ).translate(10), 2, 8, numLevel, 5);
        *)
        EVAL TestMatchPatternSmooth(
               Refn.Refine(BSpline.WaveletMask(2, 8),
                           BSpline.GeneratorMask(2), numLevel).scale(
                 FLOAT(unit, R.T)).translate(50), 2, 8, 8, numLevel, 5,
               1.0D-10);
    | Example.matchSincSmooth =>
        (*
          TestMatchPattern(
            NEW(S.T).fromArray(V.Neg(SincVector(2048, unit))^, unit - 2048),
            4, 6, numLevel, 10);
        *)
        FOR scale := 3 TO 10 DO
          WITH size = 5 * scale * unit DO
            TestMatchPattern(
              NEW(S.T).fromArray(V.Neg(ModulateReal(
                                         SincVector(size, scale * unit),
                                         FLOAT(scale * unit, R.T) * 4.0D0
                                           / 3.0D0))^, unit - size), 4, 0,
              numLevel, 20);
          END;
          (** which scale achieves best match?
             3 - 22.1834569283071
             4 -  5.45720352492108
             5 -  1.12455306018079
             6 -  0.32131075741861
             7 -  0.0526881888961695
             8 -  0.106707504091051
             9 -  3.60586130516876
            10 -  0.202799707663614
          *)
          WITH size = 5 * scale * unit DO
            TestMatchPattern(
              NEW(S.T).fromArray(V.Neg(ModulateImag(
                                         SincVector(size, scale * unit),
                                         FLOAT(scale * unit, R.T) * 4.0D0
                                           / 3.0D0))^, unit - size), 3, 1,
              numLevel, 20);
          END;
          (** which scale achieves best match?
             3 - 40.9491273229196
             4 -  9.13161843532297
             5 -  3.38459627541558
             6 -  1.7937034581564
             7 -  0.653510798955025
             8 -  0.91561743745227
             9 -  0.255038945635696
            10 -  0.476846592814738
          *)
        END;
      (*
      EVAL TestMatchPatternSmooth(
        NEW(S.T).fromArray(V.Neg(SincVector(2048, unit))^, unit - 2048),
        4, 6, numLevel, 10, 1.0D-3);
      *)
    | Example.matchGaussian =>
        (*
          FOR scale := 3 TO 10 DO
            IO.Put("scale: "&Fmt.Int(scale)&"\n");
            VAR size := 5 * scale * unit;
            BEGIN
              TestMatchPattern(
                NEW(S.T).fromArray(
                  ModulateReal(V.Neg(GaussianVector(size, scale * unit)),
                               FLOAT(scale * unit, R.T))^, unit - size),
                4, 0, numLevel, 20);
            END;
            VAR size := 5 * scale * unit;
            BEGIN
              TestMatchPattern(NEW(S.T).fromArray(
                             ModulateImag(GaussianVector(size, scale * unit),
                                          FLOAT(scale * unit, R.T))^,
                             unit - size), 3, 1, numLevel, 20);
            END;
          END;
        *)
        CONST
          scale = 6;
          size  = 5 * scale * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
                 NEW(S.T).fromArray(
                   ModulateReal(V.Neg(GaussianVector(size, scale * unit)),
                                FLOAT(scale * unit, R.T))^, unit - size),
                 4, 4, 0, numLevel, 6, 10.0D0);
        END;
    | Example.matchLongRamp =>
        (* matching a pattern with 1 vanishing moment with a wavelet of 9
           vanishing moments can't work obviously *)
        TestMatchPattern(NEW(S.T).fromArray(
                           V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,
                           unit - 1024), 3, 9, numLevel, 5);
      (*
      EVAL TestMatchPatternSmooth(
        NEW(S.T).fromArray(
          V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,32 -1024), 3, 1,
        numLevel, 5, 0.0D-4);
      *)
    | Example.matchMassPeak =>
        CONST
          clipFirst  = 15500;
          clipNumber = 2500;
        <* FATAL OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure,
                 Thread.Alerted *>
        VAR
          rd := FileRd.Open(
                  "/home/thielema/projects/industry/bruker/data/Datasets"
                    & "/T/Normal/spectrum_28_23.dat");
          data  := MF.Lex(rd);
          dataX := M.GetColumn(data, 0);
          dataY := M.GetColumn(data, 1);
          clipX := V.FromArray(SUBARRAY(dataX^, clipFirst, clipNumber));
          clipY := V.FromArray(SUBARRAY(dataY^, clipFirst, clipNumber));
        BEGIN
          PL.Init();
          PL.SetEnvironment(dataX[FIRST(dataX^)], dataX[LAST(dataX^)],
                            VFs.Min(dataY^), VFs.Max(dataY^));
          PL.PlotLines(dataX^, dataY^);

          PL.SetEnvironment(clipX[FIRST(clipX^)], clipX[LAST(clipX^)],
                            VFs.Min(clipY^), VFs.Max(clipY^));
          PL.PlotLines(clipX^, clipY^);
          PL.Exit();
        END;
    | Example.phdThesis =>
        WITH sincVec = NEW(S.T).fromArray(
                         V.Neg(SincVector(32 * unit, unit))^,
                         3 * unit - 32 * unit) DO
          TestMatchPattern(sincVec, 4, 0, numLevel, 6);
          TestMatchPattern(sincVec, 4, 2, numLevel, 6);
          TestMatchPattern(sincVec, 4, 4, numLevel, 6);
          EVAL TestMatchPatternSmooth(sincVec.translate(-3 * unit), 4, 0,
                                      0, numLevel, 6, 1.0D-2);
          FOR primalSmooth := 4 TO 8 BY 2 DO
            (*
            EVAL
              TestMatchPatternSmooth(sincVec.translate(-3 * unit), 4,
                                     primalSmooth, 0, numLevel, 6, R.Zero);
            *)
            CONST
              nameSide = ARRAY [0 .. 1] OF TEXT{"Primal", "Dual"};
              nameType = ARRAY [0 .. 1] OF TEXT{"Generator", "Wavelet"};
            VAR
              name := Fmt.F(
                        "sincBank%s_%s", Fmt.Int(4), Fmt.Int(primalSmooth));
              bank := WaveletMatchSmooth.BSplineWithPattern(
                        sincVec.translate(-3 * unit), 4, primalSmooth, 0,
                        Range.New(-6, 12), numLevel, R.Zero,
                        options := WaveletMatchSmooth.Options{maxIter := 0});
            <* FATAL Arith.Error, Thread.Alerted, Wr.Failure *>
            BEGIN
              IO.Put(
                Fmt.F("%s :: Fractional a => "
                        & "Side -> FuncType -> ShiftedSignal.T a\n", name));
              FOR i := FIRST(bank) TO LAST(bank) DO
                FOR j := FIRST(bank[i]) TO LAST(bank[i]) DO
                  IO.Put(Fmt.F("%s %s %s = %s\n", name, nameSide[i],
                               nameType[j], FmtSignalHaskell(bank[i, j])));
                END;
              END;
            END;
          END;
        END;

        WITH width = 12 * unit DO
          TestMatchPattern(
            NEW(S.T).fromArray(
              V.ArithSeq(width, -1.0D0, 2.0D0 / FLOAT(width, R.T))^,
              3 * unit - width DIV 2), 2, 0, numLevel, 6);
          TestMatchPattern(
            NEW(S.T).fromArray(
              V.ArithSeq(width, -1.0D0, 2.0D0 / FLOAT(width, R.T))^,
              3 * unit - width DIV 2), 3, 1, numLevel, 6);
          TestMatchPattern(
            NEW(S.T).fromArray(
              V.ArithSeq(width, -1.0D0, 2.0D0 / FLOAT(width, R.T))^,
              3 * unit - width DIV 2), 3, 5, numLevel, 6);
        END;
    | Example.checkVanishingMoments => CheckVanishingMoments();
    ELSE
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END ExampleWaveletMatch.
