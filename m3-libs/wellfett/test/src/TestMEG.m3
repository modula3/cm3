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
       PLPlot                       AS PL;

IMPORT Pathname, IO, Fmt, Wr, Thread, Rd, GZipRd;
IMPORT Arithmetic AS Arith;

<* UNUSED *>
PROCEDURE PlotReal (s: S.T; l: CARDINAL; ) =
  <* FATAL Arith.Error *>        (*MulPower cannot fail for integers*)
  VAR
    unit  := IIntPow.MulPower(1, 2, l);
    grid  := R.One / FLOAT(unit, R.T);
    left  := FLOAT(s.getFirst(), R.T) * grid;
    right := FLOAT(s.getLast(), R.T) * grid;
  BEGIN
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(
      left, right, VFs.Min(s.getData()^), VFs.Max(s.getData()^));
    PL.SetFGColorDiscr(2);
    PL.PlotLines(
      V.ArithSeq(s.getNumber(), FLOAT(s.getFirst(), R.T) * grid, grid)^,
      s.getData()^);
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


PROCEDURE SuppressPeriod (file: Pathname.T; ) =
  VAR rd := GZipRd.Open(file);
  BEGIN
    FOR i := 0 TO 9 DO IO.Put(Rd.GetLine(rd) & "\n"); END;
    WITH x = VF.Lex(rd, VF.LexStyle{sep := '\n'}) DO
      IO.Put(VF.Fmt(x));
    END;
    Rd.Close(rd);
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
