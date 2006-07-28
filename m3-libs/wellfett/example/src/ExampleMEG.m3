MODULE ExampleMEG;

IMPORT LongRealBasic                          AS R,
       LongRealTrans                          AS RT,
       LongRealIntegerPower                   AS RIntPow,
       LongRealComplex                        AS C,
       LongRealComplexTrans                   AS CT,
       LongRealVector                         AS V,
       LongRealVectorFast                     AS VFs,
       LongRealVectorTrans                    AS VT,
       LongRealSignal                         AS S,
       LongRealScaledSignal                   AS ScaledSignal,
       LongRealSignalIntegerPower             AS SIntPow,
       LongRealVectorFmtLex                   AS VF,
       LongRealSignalFmtLex                   AS SF,
       LongRealDyadicFilterBank               AS FilterBank,
       LongRealDyadicDiscreteWaveletTransform AS DWT,
       LongRealBSplineWavelet                 AS BSpline,
       LongRealWaveletMatchBasis              AS WMBasis,
       LongRealWaveletMatchSmooth             AS WaveletMatchSmooth,
       LongRealRefinableFunction              AS Refn,
       LongRealFFTW                           AS FFT,
       LongRealInterpolation                  AS Interpolation;

IMPORT LongRealPLPlot           AS PL,
       LongRealPLPlotStream     AS Stream,
       LongRealPLPlotFrame      AS Frame,
       LongRealPLPlotFigure     AS Figure,
       LongRealPLPlotLineStyle  AS LineStyle,
       LongRealDWTPlot          AS DWTPlot,
       LongRealScaledSignalPlot AS ScaledSignalPlot;

IMPORT Pathname, IO, Fmt, Rd, Wr, GZipRd, Lex;
IMPORT Thread, OSError, FloatMode, Atom;
IMPORT Arithmetic, Range;

PROCEDURE PlotReal (READONLY s: ARRAY OF ScaledSignal.T; ) =
  CONST magnify = 1.0D0;

  VAR
    left, right := NEW(V.T, NUMBER(s));
    min, max    := R.Zero;
    color       := 2;

  BEGIN
    FOR i := FIRST(s) TO LAST(s) DO
      WITH si = s[i] DO
        left[i] := ScaledSignal.First(si);
        right[i] := ScaledSignal.Last(si);
        min := MIN(min, VFs.Min(si.signal.getData()^));
        max := MAX(max, VFs.Max(si.signal.getData()^));
      END;
    END;
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      VFs.Min(left^), VFs.Max(right^), min / magnify, max / magnify);
    FOR i := FIRST(s) TO LAST(s) DO
      WITH abscissa = V.ArithSeq(
                        s[i].signal.getNumber(), left[i], s[i].resolution)^ DO
        PL.SetFGColorDiscr(color);
        PL.PlotLines(abscissa, s[i].signal.getData()^);
        INC(color);
      END;
    END;
  END PlotReal;



PROCEDURE CircularAutoCorrelation (READONLY x: V.TBody; ): V.T =
  VAR xc := FFT.DFTR2C1D(x);
  BEGIN
    FOR i := FIRST(xc^) TO LAST(xc^) DO
      xc[i] := C.T{CT.AbsSqr(xc[i]), R.Zero};
    END;
    RETURN FFT.DFTC2R1D(xc^, NUMBER(x) MOD 2);
  END CircularAutoCorrelation;


(* In fact what we do here is the same as pitch detection in audio
   signals. *)
PROCEDURE DetectPeriod (READONLY x: V.TBody; ): CARDINAL =
  VAR
    ac             := CircularAutoCorrelation(SUBARRAY(x, 0, 4096));
    iMax: CARDINAL := 0;
    xMax           := R.Zero;
  BEGIN
    (* PlotReal(NEW(S.T).fromArray(SUBARRAY(x^, 0, 2048)), 0);
       PlotReal(NEW(S.T).fromVector(ac), 0); *)
    FOR i := 10 TO LAST(ac^) DIV 2 DO
      (* No ABS needed since only positive correlations indicate
         similarity. *)
      IF xMax < ac[i] THEN xMax := ac[i]; iMax := i; END;
    END;
    RETURN iMax;
  END DetectPeriod;


(* k must be between 0 and 1, the higher k the slower the filter reacts on
   differences in the shape of the periodic signal but the more of the
   non-periodic signal is preserved; 0 means no effect, 1 means constant
   output *)
PROCEDURE CombFilter (READONLY x: V.TBody; period: CARDINAL; k: R.T; ):
  V.T =
  VAR
    delayLine           := V.NewZero(period);
    y                   := NEW(V.T, NUMBER(x));
    j        : CARDINAL := 0;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      WITH yi = x[i] + (delayLine[j] - x[i]) * k DO
        delayLine[j] := yi;
        y[i] := yi;
      END;
      j := (j + 1) MOD period;
    END;
    RETURN y;
  END CombFilter;

(* modulated comb filter *)
PROCEDURE CombFilterMod (READONLY x        : V.TBody;
                         READONLY periods  : V.TBody;
                                  maxPeriod: R.T;
                                  k        : R.T;     ): V.T =
  VAR
    delayLine := V.NewZero(CEILING(maxPeriod) + 10);
    y         := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      WITH period      = periods[i],
           periodFloor = FLOOR(period),
           j           = (i - periodFloor) MOD NUMBER(delayLine^),
           xi          = x[i],
           frac        = period - FLOAT(periodFloor, R.T),
           yj = (R.One - frac) * delayLine[j]
                  + frac * delayLine[(j - 1) MOD NUMBER(delayLine^)],
           yi = xi + (yj - xi) * k DO
        delayLine[i MOD NUMBER(delayLine^)] := yi;
        y[i] := yi;
      END;
    END;
    RETURN y;
  END CombFilterMod;

(* For simplicity of the computation this works only for
   NUMBER(x)>=windowSize. *)
PROCEDURE MovingAverage (READONLY x: V.TBody; windowSize: CARDINAL; ):
  V.T =
  VAR
    y   := NEW(V.T, LAST(x) + windowSize);
    acc := R.Zero;
    rec := R.One / FLOAT(windowSize, R.T);
  BEGIN
    FOR j := 0 TO windowSize - 1 DO
      acc := acc + x[j];
      y[j] := acc * rec;
    END;
    FOR j := windowSize TO LAST(x) DO
      acc := acc + x[j] - x[j - windowSize];
      y[j] := acc * rec;
    END;
    (* For NUMBER(x)<windowSize we would need an extra phase here, until
       j>=windowSize. *)
    FOR j := NUMBER(x) TO LAST(y^) DO
      acc := acc - x[j - windowSize];
      y[j] := acc * rec;
    END;
    RETURN y;
  END MovingAverage;

PROCEDURE Highpass (x: V.T; windowSize: CARDINAL; order: CARDINAL := 1; ):
  V.T =
  VAR y := x;
  BEGIN
    FOR n := 1 TO order DO y := MovingAverage(y^, windowSize); END;
    WITH z = NEW(V.T, NUMBER(x^)),
         w = SUBARRAY(y^, (windowSize * order) DIV 2, NUMBER(x^)) DO
      FOR j := FIRST(x^) TO LAST(x^) DO z[j] := x[j] - w[j]; END;
      RETURN z;
    END;
  END Highpass;

PROCEDURE Limit (READONLY x: V.TBody; limit: LONGREAL; ): V.T =
  VAR z := NEW(V.T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      z[i] := MAX(-limit, MIN(limit, x[i]));
    END;
    RETURN z;
  END Limit;

PROCEDURE LimitSignal (x: S.T; limit: LONGREAL; ): S.T =
  BEGIN
    RETURN NEW(S.T).fromVector(Limit(x.getData()^, limit), x.getFirst());
  END LimitSignal;

PROCEDURE LimitSignalRel (x: S.T; limit: LONGREAL; ): S.T =
  BEGIN
    RETURN LimitSignal(x, limit * VT.Norm2(x.getData()) / RT.SqRt(
                            FLOAT(x.getNumber(), R.T)));
  END LimitSignalRel;


PROCEDURE LoadVector (filename: Pathname.T; ): V.T
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =
  VAR
    rd := GZipRd.Open(filename);
    x  := VF.Lex(rd, VF.LexStyle{sep := '\n'});
  BEGIN
    Rd.Close(rd);
    RETURN x;
  END LoadVector;


(* Use a comb filter to remove big periodic peaks.  If 'trackPeriod' is
   TRUE, then it is assumed that the period is not constant.  Thus it is
   tracked (which works quite reliably) and a modulated comb filter is
   used. *)
PROCEDURE SuppressPeriod (filename: Pathname.T; trackPeriod: BOOLEAN; )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted,
          Arithmetic.Error} =

  CONST
    numPeriods = 2;
    (* inertia = 0.95D0; this value is good for clean shapes of the mother
       wave *)
    inertia = 0.8D0;             (* this value is better for canceling the
                                    mother wave *)

  VAR
    xOrig              := LoadVector(filename);
    x                  := Highpass(xOrig, 100, 3);
    period             := DetectPeriod(x^);
    blockSize          := numPeriods * period;
    resolution         := R.Rec(R.FromInteger(period));
    comb, residue: V.T;

  BEGIN
    PL.Init();
    IF NOT trackPeriod THEN
      comb := CombFilter(x^, period, inertia);
    ELSE
      WITH periodsIp = InterpolatePeriods(TrackPeriod(x^), NUMBER(x^)) DO
        comb :=
          CombFilterMod(x^, periodsIp^, VFs.Max(periodsIp^), inertia);
      END;
    END;
    residue := V.Sub(x, comb);

    IO.Put(Fmt.F("samples: %s\nperiod: %s\n", Fmt.Int(NUMBER(x^)),
                 Fmt.Int(period)));
    (* PlotReal(NEW(S.T).fromVector(x), 0); *)
    FOR j := 0 TO LAST(x^) BY blockSize DO
      WITH size = MIN(blockSize, NUMBER(x^) - j) DO
        PlotReal(ARRAY OF
                   ScaledSignal.T{
                   ScaledSignal.T{NEW(S.T).fromArray(
                                    SUBARRAY(x^, j, size), j), resolution},
                   ScaledSignal.T{
                     NEW(S.T).fromVector(comb).clip(j, size), resolution},
                   ScaledSignal.T{NEW(S.T).fromVector(residue).clip(
                                    j, size), resolution}});
      END;
    END;
    PL.Exit();
  END SuppressPeriod;


(* TrackPeriod can only return integer periods - should we use integer
   vectors for instead?  In principle it could also return values with
   sub-sampling precision. *)
TYPE IrregularSamples = RECORD x, y: V.T;  END;

(* Given a signal 'x' jump from peak to peak an return the positions of the
   peaks in '.x' and the period, i.e.  the distance to the previous peak in
   '.y'. *)
PROCEDURE TrackPeriod (READONLY x: V.TBody; ): IrregularSamples =

  PROCEDURE FindMax (first, number: CARDINAL; ): CARDINAL =
    VAR
      max           := R.Zero;
      i  : CARDINAL := first;
    BEGIN
      FOR j := first TO first + number - 1 DO
        IF max < ABS(x[j]) THEN max := ABS(x[j]); i := j; END;
      END;
      RETURN i;
    END FindMax;

  PROCEDURE NewNode (x, y: CARDINAL; ) =
    BEGIN
      node := NEW(NodeList, prev := node, x := x, y := y);
      INC(numNodes);
    END NewNode;

  TYPE
    NodeList = REF RECORD
                     prev: NodeList;
                     x, y: CARDINAL;
                   END;

  VAR
    period             := DetectPeriod(x);
    node    : NodeList := NIL;
    numNodes: CARDINAL := 0;
    i       : CARDINAL;

  BEGIN
    (* Find the first peak within the first period *)
    i := FindMax(0, period);
    NewNode(0, period);
    NewNode(i, period);

    (* Jump from peak to peak. *)
    WHILE i + period + (period + 1) DIV 8 <= LAST(x) DO
      WITH j = FindMax(i + period - period DIV 8, period DIV 4) DO
        period := j - i;
        i := j;
      END;
      NewNode(i, period);
    END;
    NewNode(LAST(x), period);

    (* Copy all nodes into x and y arrays *)
    VAR smp := IrregularSamples{NEW(V.T, numNodes), NEW(V.T, numNodes)};
    BEGIN
      FOR j := numNodes - 1 TO 0 BY -1 DO
        smp.x[j] := FLOAT(node.x, R.T);
        smp.y[j] := FLOAT(node.y, R.T);
        node := node.prev;
      END;
      RETURN smp;
    END;
  END TrackPeriod;

PROCEDURE InterpolatePeriods
  (READONLY periods: IrregularSamples; number: CARDINAL; ): V.T =
  VAR x := NEW(V.T, number);
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      x[i] :=
        Interpolation.CubicHermite(periods.x^, periods.y^, FLOAT(i, R.T));
    END;
    RETURN x;
  END InterpolatePeriods;


EXCEPTION TerminateFor;

(* Find a prototypic shape of a big peak by averaging all peaks we can
   find. *)
PROCEDURE AveragePeak (READONLY x: V.TBody; ): S.T =
  VAR
    periods := TrackPeriod(x);
    width := ROUND(
               VFs.Max(SUBARRAY(periods.y^, 2, NUMBER(periods.y^) - 3)));
    halfWidth           := width DIV 2;
    avrg                := V.NewZero(width);
    number   : CARDINAL := 0;

  BEGIN
    TRY
      FOR i := 2 TO LAST(periods.x^) DO
        WITH j = ROUND(periods.x[i]) DO
          IF j - halfWidth + width <= NUMBER(x) THEN
            avrg := VFs.Add(avrg^, SUBARRAY(x, j - halfWidth, width));
            INC(number);
          ELSE
            RAISE TerminateFor;
          END;
        END;
      END;
    EXCEPT
    | TerminateFor =>
    END;
    RETURN NEW(S.T).fromVector(
             V.Scale(avrg, R.One / FLOAT(number, R.T)), -halfWidth);
  END AveragePeak;




PROCEDURE ShowPeriod (filename: Pathname.T; ) RAISES {OSError.E,
                                                      FloatMode.Trap,
                                                      Lex.Error,
                                                      Rd.Failure,
                                                      Thread.Alerted} =

  CONST blockSize = 10000;

  VAR
    x         := LoadVector(filename);
    periods   := TrackPeriod(x^);
    periodsIp := InterpolatePeriods(periods, NUMBER(x^));
    abscissa  := V.ArithSeq(NUMBER(x^), R.Zero, R.One);
    minY      := VFs.Min(periods.y^);
    maxY      := VFs.Max(periods.y^);

  BEGIN
    PL.Init();
    IF FALSE THEN
      (* show all at once *)
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(
        FLOAT(FIRST(x^), R.T), FLOAT(LAST(x^), R.T), minY, maxY);
      PL.SetFGColorDiscr(2);
      PL.PlotLines(periods.x^, periods.y^);

    ELSE
      (* show block-wise *)
      FOR j := 0 TO LAST(x^) BY blockSize (* DIV 50 *) DO
        PL.SetFGColorDiscr(1);
        PL.SetEnvironment(
          FLOAT(j, R.T), FLOAT(j + blockSize - 1, R.T), minY, maxY);
        PL.SetFGColorDiscr(2);
        PL.PlotLines(periods.x^, periods.y^);
        PL.SetFGColorDiscr(3);
        PL.PlotLines(abscissa^, periodsIp^);
      END;
    END;
    PL.Exit();
  END ShowPeriod;

(* Visual comparison between the original signal and a time varying delayed
   signal.  If the period is determined properly by TrackPeriod the peaks
   of neighboured periods should match. *)
PROCEDURE ShowTranslatedMatch (filename: Pathname.T; )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =

  CONST blockSize = 1000;

  VAR
    xOrig             := LoadVector(filename);
    x                 := Highpass(xOrig, 100, 3);
    periods           := TrackPeriod(x^);
    periodsIp         := InterpolatePeriods(periods, NUMBER(x^));
    abscissa          := V.ArithSeq(NUMBER(x^), R.Zero, R.One);
    abscissaDistorted := V.ArithSeq(NUMBER(x^), R.Zero, R.One);
    minY              := VFs.Min(x^);
    maxY              := VFs.Max(x^);

  BEGIN
    (* Move the signal about one period to the left (past), this is done
       variable such that peaks should match even if the period varies. *)
    FOR j := FIRST(abscissaDistorted^) TO LAST(abscissaDistorted^) DO
      abscissaDistorted[j] := abscissaDistorted[j] - periodsIp[j];
    END;

    PL.Init();
    FOR j := FIRST(x^) TO LAST(x^) BY blockSize (* DIV 50 *) DO
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(
        FLOAT(j, R.T), FLOAT(j + blockSize - 1, R.T), minY, maxY);
      PL.SetFGColorDiscr(2);
      PL.PlotLines(abscissa^, x^);
      PL.SetFGColorDiscr(3);
      PL.PlotLines(abscissaDistorted^, x^);
    END;
    PL.Exit();
  END ShowTranslatedMatch;

TYPE BankPair = ARRAY [0 .. 1] OF FilterBank.TBody;

PROCEDURE ScaleBank (READONLY bank: BankPair; ): BankPair =
  VAR newBank: BankPair;
  BEGIN
    newBank[0, 0] := bank[0, 0].scale(RT.SqRtTwo);
    newBank[0, 1] := bank[0, 1].scale(RT.SqRtTwo);
    newBank[1, 0] := bank[1, 0].scale(RT.SqRtTwo);
    newBank[1, 1] := bank[1, 1].scale(RT.SqRtTwo);
    RETURN newBank;
  END ScaleBank;

PROCEDURE ReverseBank (READONLY bank: BankPair; ): BankPair =
  VAR revBank: BankPair;
  BEGIN
    revBank[0, 0] := bank[0, 0].reverse();
    revBank[0, 1] := bank[0, 1].reverse();
    revBank[1, 0] := bank[1, 0].reverse();
    revBank[1, 1] := bank[1, 1].reverse();
    RETURN revBank;
  END ReverseBank;


PROCEDURE Normalise (x: S.T; ): S.T =
  BEGIN
    RETURN x.scale(R.One / VT.Norm2(x.getData()));
  END Normalise;

PROCEDURE DWTFilter (filename: Pathname.T; frequency: LONGREAL; )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =
  CONST
    numLevels = 5;

    dualSmooth    = 4;
    primalSmooth  = 8;
    dualVanishing = 2;
    dualAnalysis  = FALSE;

  VAR
    x          := LoadVector(filename);
    resolution := R.One / frequency;
    clip       := NEW(S.T).fromArray(SUBARRAY(x^, 0, 2000));

    bank, revBank: BankPair;

  BEGIN
    PL.Init();
    TRY
      IF FALSE THEN
        (* CDF-wavelet base *)
        bank[0] :=
          FilterBank.TBody{BSpline.GeneratorMask(4).scale(RT.SqRtTwo),
                           BSpline.WaveletMask(4, 2).scale(RT.SqRtTwo)};
        bank[1] := FilterBank.DualToPrimal(bank[0]);

      ELSE
        <* FATAL Wr.Failure *>
        VAR
          peak               := AveragePeak(x^);
          peakClip           := peak.clip(-20, 100);
          peakClipNormalised := Normalise(peakClip.translate(-2));
        BEGIN
          (* matched wavelet base *)
          PL.StartPage();
          PlotReal(
            ARRAY OF
              ScaledSignal.T{
              ScaledSignal.T{peak, resolution},
              ScaledSignal.T{clip.clip(0, peak.getNumber()), resolution}});
          PlotReal(
            ARRAY OF ScaledSignal.T{ScaledSignal.T{peakClip, resolution}});
          PL.StopPage();

          bank :=
            WaveletMatchSmooth.BSplineWithPattern(
              peakClipNormalised, dualSmooth, primalSmooth, dualVanishing,
              Range.New(-2, 20), 1, R.Zero, (* 1.0D-10,*)
              WaveletMatchSmooth.FlagSet{WaveletMatchSmooth.Flag.Plot},
              WaveletMatchSmooth.Options{maxIter := 0});
          (* Note that the dual filters are the matched ones. *)
          bank := ScaleBank(bank);
          (* For correlation the filter must be flipped *)
          revBank := ReverseBank(bank);

          IO.Put("Dual lowpass: " & SF.Fmt(bank[0, 0]) & "\n");
          IO.Put("Primal lowpass: " & SF.Fmt(bank[1, 0]) & "\n");
        END;
      END;

      VAR
        wt   : DWT.T;
        recon: S.T;
        vanishing := SIntPow.MulPower(S.One, WMBasis.vanishingAtom,
                                      primalSmooth - dualVanishing);
        revVanishing := vanishing.reverse();

      BEGIN
        IF dualAnalysis THEN
          wt :=
            DWT.HighBandOversampledFromSignal(clip, revBank[1], numLevels);
        ELSE
          wt := DWT.HighBandUpConvolve(
                  DWT.FromSignal(clip, bank[0], numLevels), vanishing);
        END;

        PL.StartPage();
        DWTPlot.HighBandOversampledDWT(wt, clip, resolution);
        PL.StopPage();

        (* process the wavelet transform *)
        PL.StartPage();
        wt.high[0] := S.Zero;
        (* wt.high[1] := S.Zero; *)
        wt.high[2] := LimitSignalRel(wt.high[2], 1.0D0);
        wt.low := S.Zero;
        DWTPlot.HighBandOversampledDWT(wt, clip, resolution);
        PL.StopPage();

        IF dualAnalysis THEN
          recon := DWT.ToSignal(DWT.HighBandConvolveDown(wt, revVanishing),
                                revBank[0]);
        ELSE
          recon := DWT.HighBandOversampledToSignal(wt, bank[1]);
        END;
        PL.StartPage();
        PL.SetSubWindows(1, 1);
        PlotReal(
          ARRAY OF
            ScaledSignal.T{
            ScaledSignal.T{clip, resolution},
            ScaledSignal.T{
              recon.clip(clip.getFirst(), clip.getNumber()), resolution}});
        PlotReal(
          ARRAY OF
            ScaledSignal.T{
            (* ScaledSignal.T{clip.clip(clip.getFirst(), 500),
               resolution},*)
            ScaledSignal.T{recon.clip(clip.getFirst(), 500), resolution}});
        PL.StopPage();
      END;

    EXCEPT
    | Arithmetic.Error (error) =>
        IO.Put("Error:\n");
        IO.Put(Atom.ToText(error.head) & "\n");
    END;
    PL.Exit();
  END DWTFilter;


(* plots used for the SPP 1114 evaluation colloquium in Freiburg,
   2005-04-17 *)
PROCEDURE PlotMatchedWaveletSlides (path: Pathname.T; )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =

  PROCEDURE InitPS (epsFilename: Pathname.T; ) =
    BEGIN
      PL.SetDevice("ps");
      PL.SetFileName("figures/" & epsFilename & ".eps");

      PL.Init();

      PL.SetPenWidth(10);
      PL.SetCharacterHeight(R.Zero, 1.5D0);
    END InitPS;



  CONST
    frequency = 250.0D0;
    numLevels = 2;

    dualSmooth    = 4;
    primalSmooth  = 8;
    dualVanishing = 2;

  VAR
    x          := LoadVector(Pathname.Join(path, "MCB0-1304", "gz"));
    resolution := R.One / frequency;

  BEGIN
    TRY
      VAR
        peak               := AveragePeak(x^);
        peakClip           := peak.clip(-20, 100);
        peakClipNormalised := Normalise(peakClip.translate(-2));

        (* matched wavelet base *)
        bank := WaveletMatchSmooth.BSplineWithPattern(
                  peakClipNormalised, dualSmooth, primalSmooth,
                  dualVanishing, Range.New(-2, 20), 1,
                  R.Zero,        (* 1.0D-10, *)
                  options := WaveletMatchSmooth.Options{maxIter := 0});
        refinedSignal := Refn.Refine(
                           bank[1, 1].negate(), bank[1, 0], numLevels);
        waveletResolution := RIntPow.MulPower(
                               R.Two * resolution, R.Half, numLevels);
        peakClipWavAmp := peakClipNormalised.scale(
                            VT.NormInf(refinedSignal.getData())
                              / VT.NormInf(peakClipNormalised.getData()));
      BEGIN
        (* Note that the dual filters are the matched ones. *)
        bank := ScaleBank(bank);

        InitPS("meg-peak");
        PlotReal(
          ARRAY OF
            ScaledSignal.T{ScaledSignal.T{peakClipWavAmp, resolution}});
        PL.Exit();

        InitPS("meg-peak-wavelet");
        PlotReal(ARRAY OF
                   ScaledSignal.T{
                   ScaledSignal.T{peakClipWavAmp, resolution},
                   ScaledSignal.T{refinedSignal, waveletResolution}});
        PL.Exit();

        InitPS("meg-wavelet");
        PlotReal(ARRAY OF
                   ScaledSignal.T{
                   ScaledSignal.T{refinedSignal, waveletResolution}});
        PL.Exit();
      END;

    EXCEPT
    | Arithmetic.Error (error) =>
        IO.Put("Error:\n");
        IO.Put(Atom.ToText(error.head) & "\n");
    END;
  END PlotMatchedWaveletSlides;


PROCEDURE PlotMatchedWaveletPhDThesis (path: Pathname.T; )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =

  (* creating a monolithic graphic with all levels of the transformation is
     a pain in the ass with PLPlot *)
  <* UNUSED *>
  PROCEDURE WriteDWTMonolith
    (wt: DWT.T; resolution: LONGREAL; name: TEXT; ) =
    BEGIN
      WITH stream = NEW(Stream.PostScript).init(
                      Pathname.Join(
                        figurePath, "SQUIDmMCGDWT" & name, "eps"),
                      portrait := TRUE),
           (* stream = NEW(Stream.XWindow).init(), *)
           frame = NEW(DWTPlot.T).init(
                     DWTPlot.HighBandOversampledDWTToSigSeq(wt, resolution),
                     sizeX := 1.6D0, sizeY := 0.2D0) DO
        (* PL.SetPageParam seems to have no effect *)
        (* changing the window size or aspect with these commands means
           distortion of the characters *)
        (**
        PL.SetWindowDevice(0.0D0, 1.0D0, 0.0D0, 0.0D0);
        PL.SetWindowPlot(0.0D0, 0.0D0, 1.0D0, 1.0D0);
        *)
        stream.setPenWidth(penWidth);
        stream.setOrientation(orientation);
        stream.setCharacterRelHeight(characterRelHeight);
        stream.put(frame);
        stream.exit();
      END;
    END WriteDWTMonolith;

  PROCEDURE ExtractLowBand (wt: DWT.T; ): DWT.T =
    VAR wtBand := DWT.T{wt.low, NEW(REF ARRAY OF S.T, NUMBER(wt.high^))};
    BEGIN
      FOR i := FIRST(wtBand.high^) TO LAST(wtBand.high^) DO
        wtBand.high[i] := S.Zero;
      END;
      RETURN wtBand;
    END ExtractLowBand;

  PROCEDURE ExtractHighBand (wt: DWT.T; band: CARDINAL; ): DWT.T =
    VAR wtBand := DWT.T{S.Zero, NEW(REF ARRAY OF S.T, NUMBER(wt.high^))};
    BEGIN
      FOR i := FIRST(wtBand.high^) TO LAST(wtBand.high^) DO
        wtBand.high[i] := S.Zero;
      END;
      wtBand.high[band] := wt.high[band];
      RETURN wtBand;
    END ExtractHighBand;

  (* generate several images for the DWT *)
  PROCEDURE WriteDWT
    (wt: DWT.T; sig: S.T; resolution: LONGREAL; source, type: TEXT; ) =
    VAR
      seq   := DWTPlot.HighBandOversampledDWTToSigSeq(wt, resolution);
      scSig := ScaledSignal.T{sig, resolution};
      frames: REF ARRAY OF Frame.Single;

    BEGIN
      seq.addlo(scSig);
      frames :=
        DWTPlot.SigSeqToFrameArr(seq, sizeX := 1.6D0, sizeY := 0.2D0);
      FOR i := FIRST(frames^) TO LAST(frames^) DO
        WITH stream = NEW(Stream.PostScript).init(
                        Pathname.Join(figurePath, "SQUID" & source & "DWT"
                                                    & type & Fmt.Int(i),
                                      "eps")),
             frame = frames[i] DO
          stream.setPenWidth(penWidth);
          stream.setOrientation(orientation);
          stream.setCharacterRelHeight(characterRelHeight);
          frame.setRelSizeX(wideRelSizeX);
          frame.setRelSizeY(wideRelSizeY);
          stream.put(frame);
          stream.exit();
        END;
      END;
    END WriteDWT;

  PROCEDURE WriteIDWT
    (x: S.T; resolution: LONGREAL; source, type: TEXT; ) =
    BEGIN
      WITH stream = NEW(Stream.PostScript).init(Pathname.Join(
                                                  figurePath,
                                                  "SQUID" & source & "IDWT"
                                                    & type, "eps")),
           frame = NEW(Frame.Single).init(
                     ARRAY OF
                       Figure.T{NEW(ScaledSignalPlot.T).init(
                                  ScaledSignal.T{x, resolution})}) DO
        stream.setPenWidth(penWidth);
        stream.setOrientation(orientation);
        stream.setCharacterRelHeight(characterRelHeight);
        frame.setRelSizeX(wideRelSizeX);
        frame.setRelSizeY(wideRelSizeY);
        stream.put(frame);
        stream.exit();
      END;
    END WriteIDWT;


  CONST
    figurePath   = "/home/thielema/projects/paper/thesis/figures/plplot";
    frequency    = 250.0D0;
    numLevels    = 2;
    numDWTLevels = 5;

    dualSmooth    = 4;
    primalSmooth  = 8;
    dualVanishing = 2;

    inertia       = 0.8D0;
    amplification = 1.0D12;

    start  = 2500;
    length = 1000;

    orientation        = 1;
    characterRelHeight = 0.6D0;
    penWidth           = 2;
    relSizeX           = 0.6D0;
    relSizeY           = 0.4D0;

    wideRelSizeX = 0.7D0;
    wideRelSizeY = 0.15D0;

  VAR
    x := LoadVector(Pathname.Join(path, "MCB0-1304", "gz"));
    clip := NEW(S.T).fromArray(SUBARRAY(x^, start, length), start).scale(
              amplification);
    resolution := R.One / frequency;

  BEGIN
    WITH stream = NEW(Stream.PostScript).init(
                    Pathname.Join(figurePath, "SQUIDOriginal", "eps")),
         frame = NEW(Frame.Single).init(
                   ARRAY OF
                     Figure.T{NEW(ScaledSignalPlot.T).init(
                                ScaledSignal.T{clip, resolution})}) DO
      stream.setPenWidth(penWidth);
      stream.setOrientation(orientation);
      stream.setCharacterRelHeight(characterRelHeight);
      frame.setRelSizeX(wideRelSizeX);
      frame.setRelSizeY(wideRelSizeY);
      stream.put(frame);
      stream.exit();
    END;

    VAR
      xHigh     := Highpass(V.Scale(x, amplification), 100, 3);
      periodsIp := InterpolatePeriods(TrackPeriod(xHigh^), NUMBER(xHigh^));
      comb := CombFilterMod(
                xHigh^, periodsIp^, VFs.Max(periodsIp^), inertia);
      residue := V.Sub(xHigh, comb);

    BEGIN
      WITH stream = NEW(Stream.PostScript).init(
                      Pathname.Join(figurePath, "SQUIDCombFilter", "eps")),
           frame = NEW(Frame.Single).init(
                     ARRAY OF
                       Figure.T{
                       NEW(ScaledSignalPlot.T).init(
                         ScaledSignal.T{NEW(S.T).fromVector(residue).clip(
                                          start, length), resolution})}) DO
        stream.setPenWidth(penWidth);
        stream.setOrientation(orientation);
        stream.setCharacterRelHeight(characterRelHeight);
        frame.setRelSizeX(wideRelSizeX);
        frame.setRelSizeY(wideRelSizeY);
        stream.put(frame);
        stream.exit();
      END;
    END;

    TRY
      VAR
        peak               := AveragePeak(x^);
        peakClip           := peak.clip(-20, 100);
        peakClipNormalised := Normalise(peakClip.translate(-2));

        (* matched wavelet base *)
        bank := WaveletMatchSmooth.BSplineWithPattern(
                  peakClipNormalised, dualSmooth, primalSmooth,
                  dualVanishing, Range.New(-2, 20), numLevels - 1,
                  R.Zero,        (* 1.0D-10, *)
                  options := WaveletMatchSmooth.Options{maxIter := 0});
        refinedSignal := Refn.Refine(
                           bank[1, 1].negate(), bank[1, 0], numLevels);
        waveletResolution := RIntPow.MulPower(
                               R.Two * resolution, R.Half, numLevels);
        peakClipWavAmp := peakClipNormalised.scale(
                            VT.NormInf(refinedSignal.getData())
                              / VT.NormInf(peakClipNormalised.getData()));

        vanishing := SIntPow.MulPower(S.One, WMBasis.vanishingAtom,
                                      primalSmooth - dualVanishing);

        revBank     : BankPair;
        revVanishing           := vanishing.reverse();

      <* FATAL Wr.Failure *>
      BEGIN
        (* Note that the dual filters are the matched ones. *)
        bank := ScaleBank(bank);
        bank[0, 1] := bank[0, 1].translate(-2);
        bank[1, 1] := bank[1, 1].translate(2);
        revBank := ReverseBank(bank);

        IO.Put("Dual lowpass: " & SF.Fmt(bank[0, 0]) & "\n\n");
        IO.Put("Dual highpass: " & SF.Fmt(bank[0, 1]) & "\n\n");
        IO.Put("Primal lowpass: " & SF.Fmt(bank[1, 0]) & "\n\n");
        IO.Put("Primal highpass: " & SF.Fmt(bank[1, 1]) & "\n\n");

        WITH stream = NEW(Stream.PostScript).init(
                        Pathname.Join(
                          figurePath, "SQUIDmMCGWaveMatched", "eps")),
             frame = NEW(Frame.Single).init(
                       ARRAY OF
                         Figure.T{
                         NEW(ScaledSignalPlot.T).init(
                           ScaledSignal.T{peakClipWavAmp, resolution}),
                         NEW(ScaledSignalPlot.T).init(
                           ScaledSignal.T{
                             refinedSignal.translate(6), waveletResolution},
                           style := NEW(LineStyle.Default,
                                        style := PL.LineStyle.LongDash))}) DO
          stream.setPenWidth(penWidth);
          stream.setOrientation(orientation);
          stream.setCharacterRelHeight(characterRelHeight);
          frame.setRelSizeX(relSizeX);
          frame.setRelSizeY(relSizeY);
          stream.put(frame);
          stream.exit();
        END;

        (* for testing decompose a single mMCG peak *)
        CONST sourceName = "mMCG";
        VAR
          wt   : DWT.T;
          recon: S.T;
        BEGIN
          (* peakClipNormalised := S.One; *)
          (* correlate with matched wavelet *)
          wt := DWT.HighBandOversampledFromSignal(
                  peakClipNormalised, revBank[1], numDWTLevels);
          WriteDWT(wt, peakClipNormalised, resolution, sourceName, "Dual");
          recon := DWT.ToSignal(DWT.HighBandConvolveDown(wt, revVanishing),
                                revBank[0]);
          WriteIDWT(recon, resolution, sourceName, "Dual");

          (* reconstruct with matched wavelet *)
          wt := DWT.HighBandUpConvolve(DWT.FromSignal(
                                         peakClipNormalised, bank[0],
                                         numDWTLevels), vanishing);
          WriteDWT(
            wt, peakClipNormalised, resolution, sourceName, "Primal");
          recon := DWT.HighBandOversampledToSignal(wt, bank[1]);
          WriteIDWT(recon, resolution, sourceName, "Primal");
        END;

        (* testing shift sensitivity by decomposing a shifted peak *)
        CONST sourceName = "mMCGShift";
        VAR
          wt       : DWT.T;
          peakTrans        := peakClipNormalised.translate(1);
        BEGIN
          (* correlate with matched wavelet *)
          wt := DWT.HighBandOversampledFromSignal(
                  peakTrans, revBank[1], numDWTLevels);
          WriteDWT(wt, peakTrans, resolution, sourceName, "Dual");

          (* reconstruct with matched wavelet *)
          wt :=
            DWT.HighBandUpConvolve(
              DWT.FromSignal(peakTrans, bank[0], numDWTLevels), vanishing);
          WriteDWT(wt, peakTrans, resolution, sourceName, "Primal");
        END;

        (* synthesize a wavelet from a wavelet coefficient *)
        WITH wt = DWT.T{S.Zero, NEW(REF ARRAY OF S.T, 2)} DO
          wt.high[0] := S.Zero;
          wt.high[1] := S.One;
          WriteIDWT(DWT.HighBandOversampledToSignal(wt, bank[1]),
                    resolution, "mMCG", "Pattern");
        END;

        (* decompose a SQUID signal *)
        CONST sourceName = "Signal";
        VAR
          wt, wtBand: DWT.T;
          recon     : S.T;
        BEGIN
          (* correlate with matched wavelet *)
          wt := DWT.HighBandOversampledFromSignal(
                  clip, revBank[1], numDWTLevels);
          WriteDWT(wt, clip, resolution, sourceName, "Dual");

          (* create signals that are represented by each band *)
          FOR i := 0 TO numDWTLevels DO
            IF i < numDWTLevels THEN
              wtBand := ExtractHighBand(wt, i);
            ELSE
              wtBand := ExtractLowBand(wt);
            END;
            recon := DWT.ToSignal(DWT.HighBandConvolveDown(
                                    wtBand, revVanishing), revBank[0]);
            WriteIDWT(recon.clip(clip.getFirst(), clip.getNumber()),
                      resolution, sourceName, "Dual" & Fmt.Int(i));
          END;

          (*
          wt.high[0] := S.Zero;
          wt.high[1] := S.Zero;
          wt.high[2] := wt.high[2].scale(0.15D0);
          wt.high[3] := wt.high[3].scale(0.15D0);
          *)
          wt.high[0] := LimitSignalRel(wt.high[0], 0.1D0);
          wt.high[1] := LimitSignalRel(wt.high[1], 0.1D0);
          wt.high[2] := LimitSignalRel(wt.high[2], 0.1D0);
          wt.high[3] := LimitSignalRel(wt.high[3], 0.1D0);
          wt.low := S.Zero;
          recon := DWT.ToSignal(DWT.HighBandConvolveDown(wt, revVanishing),
                                revBank[0]);
          WriteIDWT(recon.clip(clip.getFirst(), clip.getNumber()),
                    resolution, sourceName, "Dual");

          (* reconstruct with matched wavelet *)
          wt := DWT.HighBandUpConvolve(
                  DWT.FromSignal(clip, bank[0], numDWTLevels), vanishing);
          WriteDWT(wt, clip, resolution, sourceName, "Primal");

          (* create signals that are represented by each band *)
          FOR i := 0 TO numDWTLevels DO
            IF i < numDWTLevels THEN
              wtBand := ExtractHighBand(wt, i);
            ELSE
              wtBand := ExtractLowBand(wt);
            END;
            recon := DWT.HighBandOversampledToSignal(wtBand, bank[1]);
            WriteIDWT(recon.clip(clip.getFirst(), clip.getNumber()),
                      resolution, sourceName, "Primal" & Fmt.Int(i));
          END;

          (*
          wt.high[0] := S.Zero;
          wt.high[1] := S.Zero;
          wt.high[2] := wt.high[2].scale(0.15D0);
          wt.high[3] := wt.high[3].scale(0.15D0);
          *)
          wt.high[0] := LimitSignalRel(wt.high[0], 0.1D0);
          wt.high[1] := LimitSignalRel(wt.high[1], 0.1D0);
          wt.high[2] := LimitSignalRel(wt.high[2], 0.1D0);
          wt.high[3] := LimitSignalRel(wt.high[3], 0.1D0);
          wt.low := S.Zero;
          recon := DWT.HighBandOversampledToSignal(wt, bank[1]);
          WriteIDWT(recon.clip(clip.getFirst(), clip.getNumber()),
                    resolution, sourceName, "Primal");
        END;
      END;

    EXCEPT
    | Arithmetic.Error (error) =>
        IO.Put("Error:\n");
        IO.Put(Atom.ToText(error.head) & "\n");
    END;
  END PlotMatchedWaveletPhDThesis;



PROCEDURE Run () =
  CONST
    Path ="/localdata/meg/acoustic";
    (* Path = "/home/thielema/data/meg/acoustic"; *)

    (* works good: MCB0, MCC0, MCI0 *)
    (* two large peaks of different frequency: MCE0, MCG0 *)
    FileName = "MCB0-1304";

  <* FATAL ANY *>
  VAR
    filePath := Pathname.Join(Path, FileName, "gz");
  BEGIN
    CASE 5 OF
    | 0 => ShowPeriod(filePath);
    | 1 => SuppressPeriod(filePath, TRUE);
    | 2 => ShowTranslatedMatch(filePath);
    | 3 => DWTFilter(filePath, 250.0D0);
    | 4 => PlotMatchedWaveletSlides(Path);
    | 5 => PlotMatchedWaveletPhDThesis(Path);
    ELSE
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END ExampleMEG.
