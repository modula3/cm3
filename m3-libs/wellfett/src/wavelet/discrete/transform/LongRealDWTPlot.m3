MODULE LongRealDWTPlot;

IMPORT LongRealBasic           AS R,
       LongRealVector          AS V,
       LongRealVectorFast      AS VFs,
       LongRealSignal          AS S,
       LongRealScaledSignal    AS ScaledSignal,
       LongRealScaledSignalSeq AS ScaledSignalSeq,
       LongRealSignalFmtLex    AS SF;

IMPORT LongRealDyadicDiscreteWaveletTransform AS DWTransform;

IMPORT LongRealPLPlot           AS PL,
       LongRealPLPlotFrame      AS Frame,
       LongRealPLPlotFigure     AS Figure,
       LongRealPLPlotLineStyle  AS LineStyle,
       LongRealScaledSignalPlot AS SigPlot;


PROCEDURE SignalSequenceDomain (xs: ScaledSignalSeq.T; ): Figure.Range =
  VAR
    x0    := xs.get(0);
    left  := ScaledSignal.First(x0);
    right := ScaledSignal.Last(x0);
  BEGIN
    FOR i := 1 TO xs.size() - 1 DO
      WITH x = xs.get(i) DO
        left := MIN(left, ScaledSignal.First(x));
        right := MAX(right, ScaledSignal.Last(x));
      END;
    END;
    RETURN Figure.Range{left, right};
  END SignalSequenceDomain;

PROCEDURE PlotSignal (READONLY x: ScaledSignal.T; left, right: R.T; ) =
  VAR
    xMin := VFs.Min(x.signal.getData()^);
    xMax := VFs.Max(x.signal.getData()^);
  BEGIN
    IF xMin = xMax THEN xMin := xMin - R.One; xMax := xMax + R.One; END;
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(left, right, xMin, xMax);
    PL.SetFGColorDiscr(2);
    PL.PlotLines(V.ArithSeq(x.signal.getNumber(), ScaledSignal.First(x),
                            x.resolution)^, x.signal.getData()^);
  END PlotSignal;

PROCEDURE PlotSignalSequence (xs: ScaledSignalSeq.T; ) =
  VAR domain := SignalSequenceDomain(xs);
  BEGIN
    PL.SetSubWindows(1, xs.size());
    FOR i := 0 TO xs.size() - 1 DO
      PlotSignal(xs.get(i), domain.min, domain.max);
    END;
  END PlotSignalSequence;


PROCEDURE DWTToSigSeq (wt: DWTransform.T; samplePeriod: R.T; ):
  ScaledSignalSeq.T =
  VAR
    x   := NEW(ScaledSignalSeq.T).init();
    res := samplePeriod;

  BEGIN
    FOR i := FIRST(wt.high^) TO LAST(wt.high^) DO
      res := res * R.Two;
      x.addhi(ScaledSignal.T{wt.high[i], res});
    END;
    x.addhi(ScaledSignal.T{wt.low, res});
    RETURN x;
  END DWTToSigSeq;

PROCEDURE HighBandOversampledDWTToSigSeq
  (wt: DWTransform.T; samplePeriod: R.T; ): ScaledSignalSeq.T =
  VAR
    x   := NEW(ScaledSignalSeq.T).init();
    res := samplePeriod;
  BEGIN
    FOR i := FIRST(wt.high^) TO LAST(wt.high^) DO
      x.addhi(ScaledSignal.T{wt.high[i], res});
      res := res * R.Two;
    END;
    x.addhi(ScaledSignal.T{wt.low, res});
    RETURN x;
  END HighBandOversampledDWTToSigSeq;

PROCEDURE ShiftInvariantDWTToSigSeq
  (wt: DWTransform.T; samplePeriod: R.T; ): ScaledSignalSeq.T =
  VAR x := NEW(ScaledSignalSeq.T).init();
  BEGIN
    FOR i := FIRST(wt.high^) TO LAST(wt.high^) DO
      x.addhi(ScaledSignal.T{wt.high[i], samplePeriod});
    END;
    x.addhi(ScaledSignal.T{wt.low, samplePeriod});
    RETURN x;
  END ShiftInvariantDWTToSigSeq;



PROCEDURE DWT (wt: DWTransform.T; y: S.T; samplePeriod: R.T; ) =
  VAR x := DWTToSigSeq(wt, samplePeriod);
  BEGIN
    IF y # NIL THEN x.addlo(ScaledSignal.T{y, samplePeriod}); END;
    PlotSignalSequence(x);
  END DWT;

PROCEDURE HighBandOversampledDWT
  (wt: DWTransform.T; y: S.T; samplePeriod: R.T; ) =
  VAR x := HighBandOversampledDWTToSigSeq(wt, samplePeriod);
  BEGIN
    IF y # NIL THEN x.addlo(ScaledSignal.T{y, samplePeriod}); END;
    PlotSignalSequence(x);
  END HighBandOversampledDWT;

PROCEDURE ShiftInvariantDWT
  (wt: DWTransform.T; y: S.T; samplePeriod: R.T; ) =
  VAR x := ShiftInvariantDWTToSigSeq(wt, samplePeriod);
  BEGIN
    IF y # NIL THEN x.addlo(ScaledSignal.T{y, samplePeriod}); END;
    PlotSignalSequence(x);
  END ShiftInvariantDWT;


PROCEDURE SigSeqToFrameArr (xs        : ScaledSignalSeq.T;
                            color     : CARDINAL;
                            style     : LineStyle.T;
                            frameColor: CARDINAL;
                            sizeX     : LONGREAL;
                            sizeY     : LONGREAL;          ):
  REF ARRAY OF Frame.Single =
  VAR
    frames := NEW(REF ARRAY OF Frame.Single, xs.size());
    domain := SignalSequenceDomain(xs);
  BEGIN
    FOR i := FIRST(frames^) TO LAST(frames^) DO
      WITH frame = NEW(Frame.Single).init(
                     ARRAY OF
                       Figure.T{
                       NEW(SigPlot.T).init(
                         xs.get(i), color := color, style := style)},
                     color := frameColor) DO
        frame.setRangeX(domain);
        frame.setRelSizeX(sizeX);
        frame.setRelSizeY(sizeY);
        frames[i] := frame;
      END;
    END;
    RETURN frames;
  END SigSeqToFrameArr;



REVEAL T = Public BRANDED OBJECT OVERRIDES init := Init; END;

PROCEDURE Init (SELF      : T;
                xs        : ScaledSignalSeq.T;
                color     : CARDINAL;
                style     : LineStyle.T;
                frameColor: CARDINAL;
                sizeX     : LONGREAL;
                sizeY     : LONGREAL;          ): T =
  VAR
    framesRev := NEW(REF ARRAY OF ARRAY OF Frame.T, 1, xs.size());
    frames := SigSeqToFrameArr(xs, color, style, frameColor, sizeX, sizeY);
  BEGIN
    FOR i := FIRST(frames^) TO LAST(frames^) DO
      framesRev[0, i] := frames[LAST(frames^) - i];
    END;
    RETURN Frame.Matrix.init(SELF, framesRev^);
  END Init;

BEGIN
END LongRealDWTPlot.
