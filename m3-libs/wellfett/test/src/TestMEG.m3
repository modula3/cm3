MODULE TestMEG;

IMPORT LongRealBasic                AS R,
       LongRealComplex              AS C,
       LongRealComplexTrans         AS CT,
       LongRealVector               AS V,
       LongRealVectorFast           AS VFs,
       LongRealVectorTrans          AS VT,
       LongRealComplexVector        AS CV,
       LongRealComplexVectorBasic   AS CVB,
       LongRealComplexVectorSupport AS CVS,
       LongRealComplexVectorTrans   AS CVT,
       LongRealComplexVectorFmtLex  AS CVF,
       LongRealComplexInterpolation AS CVIp,
       LongRealSignal               AS S,
       LongRealComplexSignal        AS CS,
       LongRealBSplineWavelet       AS BSpl,
       LongRealFmtLex               AS RF,
       LongRealVectorFmtLex         AS VF,
       LongRealSignalFmtLex         AS SF,
       LongRealRefinableFunc        AS Refn,
       LongRealFFTW                 AS FFT,
       LongRealIntegerPower         AS RIntPow,
       Integer32IntegerPower        AS IIntPow,
       LongRealPLPlot                       AS PL;

IMPORT Pathname, IO, Fmt, Wr, Thread, Rd, GZipRd;
IMPORT Arithmetic AS Arith;

TYPE
  ScaledRealSignal = RECORD
                       sig: S.T;  (* real valued signal *)
                       res: R.T;  (* resolution, that is the width of a
                                     peak represented by a value *)
                     END;

PROCEDURE PlotReal (READONLY s: ARRAY OF ScaledRealSignal; ) =
  CONST magnify = 1.0D0;

  VAR
    left, right := NEW(V.T, NUMBER(s));
    min, max    := R.Zero;
    color       := 2;

  BEGIN
    FOR i := FIRST(s) TO LAST(s) DO
      WITH si = s[i] DO
        left[i] := FLOAT(si.sig.getFirst(), R.T) * si.res;
        right[i] := FLOAT(si.sig.getLast(), R.T) * si.res;
        min := MIN(min, VFs.Min(si.sig.getData()^));
        max := MAX(max, VFs.Max(si.sig.getData()^));
      END;
    END;
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      VFs.Min(left^), VFs.Max(right^), min / magnify, max / magnify);
    FOR i := FIRST(s) TO LAST(s) DO
      WITH abscissa = V.ArithSeq(s[i].sig.getNumber(), left[i], s[i].res)^ DO
        PL.SetFGColorDiscr(color);
        PL.PlotLines(abscissa, s[i].sig.getData()^);
        INC(color);
      END;
    END;
  END PlotReal;

TYPE
  ScaledComplexSignal = RECORD
                          sig: CS.T;  (* complex valued signal *)
                          res: R.T;  (* resolution, that is the width of a
                                        peak represented by a value *)
                        END;

  VectorPair = RECORD re, im: V.T;  END;

PROCEDURE SplitComplexVector (READONLY x: CV.TBody; ): VectorPair =
  VAR v := VectorPair{NEW(V.T, NUMBER(x)), NEW(V.T, NUMBER(x))};
  BEGIN
    FOR k := FIRST(x) TO LAST(x) DO
      v.re[k] := x[k].re;
      v.im[k] := x[k].im;
    END;
    RETURN v;
  END SplitComplexVector;

PROCEDURE PlotComplex (READONLY s: ARRAY OF ScaledComplexSignal; ) =
  CONST magnify = 1.0D0;

  VAR
    v           := NEW(REF ARRAY OF VectorPair, NUMBER(s));
    left, right := NEW(V.T, NUMBER(s));
    min, max    := R.Zero;
    color       := 2;

  BEGIN
    FOR i := FIRST(s) TO LAST(s) DO
      left[i] := FLOAT(s[i].sig.getFirst(), R.T) * s[i].res;
      right[i] := FLOAT(s[i].sig.getLast(), R.T) * s[i].res;
      v[i] := SplitComplexVector(s[i].sig.getData()^);
      min := MIN(min, VFs.Min(v[i].re^));
      min := MIN(min, VFs.Min(v[i].im^));
      max := MAX(max, VFs.Max(v[i].re^));
      max := MAX(max, VFs.Max(v[i].im^));
    END;
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      VFs.Min(left^), VFs.Max(right^), min / magnify, max / magnify);
    FOR i := FIRST(s) TO LAST(s) DO
      WITH abscissa = V.ArithSeq(s[i].sig.getNumber(), left[i], s[i].res)^ DO
        PL.SetFGColorDiscr(color);
        PL.PlotLines(abscissa, v[i].im^);
        PL.SetFGColorDiscr(color);
        PL.PlotLines(abscissa, v[i].re^);
        INC(color);
      END;
    END;
  END PlotComplex;


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
PROCEDURE DetectPeriod (x: V.T; ): CARDINAL =
  VAR
    ac             := CircularAutoCorrelation(SUBARRAY(x^, 0, 4096));
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

PROCEDURE SuppressPeriod (file: Pathname.T; ) =
  CONST numPeriods = 10;

  VAR
    rd         := GZipRd.Open(file);
    x          := VF.Lex(rd, VF.LexStyle{sep := '\n'});
    period     := DetectPeriod(x);
    blockSize  := numPeriods * period;
    resolution := R.Rec(R.FromInteger(period));
    comb       := CombFilter(x^, period, 0.8D0);
    residue    := V.Sub(x, comb);

  BEGIN
    Rd.Close(rd);
    IO.Put(Fmt.F("period: %s\n", Fmt.Int(period)));
    (* PlotReal(NEW(S.T).fromVector(x), 0); *)
    FOR j := 0 TO LAST(x^) BY blockSize DO
      WITH size = MIN(blockSize, NUMBER(x^) - j) DO
        PlotReal(
          ARRAY OF
            ScaledRealSignal{
            ScaledRealSignal{
              NEW(S.T).fromArray(SUBARRAY(x^, j, size), j), resolution},
            ScaledRealSignal{
              NEW(S.T).fromVector(comb).clip(j, size), resolution},
            ScaledRealSignal{
              NEW(S.T).fromVector(residue).clip(j, size), resolution}});
      END;
    END;
  END SuppressPeriod;


PROCEDURE Test () =
  BEGIN
    PL.Init();
    CASE 0 OF
    | 0 => SuppressPeriod("/localdata/meg/acoustic/MCB0-1304.gz");
    ELSE
      <* ASSERT FALSE *>
    END;
    PL.Exit();
  END Test;

BEGIN
END TestMEG.
