MODULE Statistic;
(* Arithmetic for Modula-3, see doc for details *)
IMPORT Arithmetic AS Arith;
IMPORT LongRealBasic AS R, LongRealTrans AS RT, SpecialFunction AS SF;

CONST Module = "Statistic.";


PROCEDURE FromData (READONLY data: ARRAY OF R.T; ): T =
  (* using the 2 pass approach *)
  <* UNUSED *>
  CONST
    ftn = Module & "describe";
  VAR
    n           := FLOAT(NUMBER(data), R.T);
    sum         := R.Zero;
    sumdelta    := R.Zero;
    delta       := R.Zero;
    tmp         := R.Zero;
    r       : T;

  BEGIN
    <* ASSERT NUMBER(data) >= 2, "need >=2 data points for moment" *>

    (*---pass 1---*)
    r.min := +RT.Huge;
    r.max := -RT.Huge;
    FOR i := FIRST(data) TO LAST(data) DO
      IF data[i] < r.min THEN r.min := data[i]; END;
      IF data[i] > r.max THEN r.max := data[i]; END;
      sum := sum + data[i];
    END;
    r.avrgVar.avrg := sum / n;

    (*---pass 2---*)
    r.adev := R.Zero;
    r.avrgVar.var := R.Zero;
    r.skew := R.Zero;
    r.kurt := R.Zero;
    FOR i := FIRST(data) TO LAST(data) DO
      delta := data[i] - r.avrgVar.avrg;
      sumdelta := sumdelta + delta;
      r.adev := r.adev + ABS(delta);
      tmp := delta * delta;
      r.avrgVar.var := r.avrgVar.var + tmp;
      tmp := tmp * delta;
      r.skew := r.skew + tmp;
      tmp := tmp * delta;
      r.kurt := r.kurt + tmp;
    END;
    r.adev := r.adev / n;

    (*---correct avrgVar.var---*)
    r.avrgVar.var :=
      (r.avrgVar.var - sumdelta * sumdelta / n) / (n - R.One);

    (*---calculate moments---*)
    r.sdev := RT.SqRt(r.avrgVar.var);
    IF r.avrgVar.var > RT.Tiny THEN
      r.skew := r.skew / (n * r.avrgVar.var * r.sdev);
      r.kurt := (r.kurt / (n * r.avrgVar.var * r.avrgVar.var)) - 3.0D0;
    ELSE
      r.skew := R.Zero;
      r.kurt := R.Zero;
    END;

    RETURN r;
  END FromData;


PROCEDURE ComputeAvrgVar (READONLY data: ARRAY OF R.T; ): AvrgVar =
  VAR
    n                             := FLOAT(NUMBER(data), R.T);
    sum, sumdelta, delta: R.T;
    avrgVar             : AvrgVar;
  BEGIN
    sum := R.Zero;
    FOR i := FIRST(data) TO LAST(data) DO sum := sum + data[i]; END;
    avrgVar.avrg := sum / n;
    sumdelta := R.Zero;
    avrgVar.var := R.Zero;
    FOR i := FIRST(data) TO LAST(data) DO
      delta := data[i] - avrgVar.avrg;
      sumdelta := sumdelta + delta;
      avrgVar.var := avrgVar.var + sumdelta * sumdelta;
    END;
    avrgVar.var := (avrgVar.var - sumdelta * sumdelta / n) / (n - R.One);
    RETURN avrgVar;
  END ComputeAvrgVar;

PROCEDURE TTest (READONLY data1, data2: ARRAY OF R.T; ): TTestResult
  RAISES {Arith.Error} =
  (* Given data and data2 equal length R.Arrays, find t, which shows how
     close the means are, and find prob, which is small if this similarity
     is unlikely to be due to chance.  Note that their variances need to be
     similar. *)
  <* UNUSED *>
  CONST
    ftn = Module & "TTest";
  VAR
    n1            := FLOAT(NUMBER(data1), R.T);
    n2            := FLOAT(NUMBER(data2), R.T);
    sd, df  : R.T;
    vardiff : R.T;
    avrgVar1      := ComputeAvrgVar(data1);
    avrgVar2      := ComputeAvrgVar(data2);

  BEGIN
    vardiff := ABS((avrgVar1.var - avrgVar2.var) / avrgVar2.var);
    IF vardiff > 5.0D0 THEN
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    END;
    df := n1 + n2 - R.Two;
    sd :=
      RT.SqRt(((n1 - R.One) * avrgVar1.var + (n2 - R.One) * avrgVar2.var)
                / df * (R.One / n1 + R.One / n2));
    WITH t = ABS((avrgVar1.avrg - avrgVar2.avrg) / sd) DO
      RETURN TTestResult{t := t, prob :=
                         SF.BetaI(0.5D0 * df, 0.5D0, df / (df + t * t))};
    END;
  END TTest;


PROCEDURE FTest (READONLY data1, data2: ARRAY OF R.T; ): FTestResult
  RAISES {Arith.Error} =
  (* do F-test, returning F and the probability that a difference between
     vars is due to chance *)
  <* UNUSED *>
  CONST
    ftn = Module & "FTest";
  VAR
    df1, df2: R.T;
    var1                  := ComputeAvrgVar(data1).var;
    var2                  := ComputeAvrgVar(data2).var;
    result  : FTestResult;
  BEGIN
    IF var1 < RT.Tiny OR var2 < RT.Tiny THEN
      (* vars cannot = 0 *)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    END;
    IF var2 > var1 THEN
      result.f := var2 / var1;
      df1 := FLOAT(NUMBER(data2) - 1, R.T);
      df2 := FLOAT(NUMBER(data1) - 1, R.T);
    ELSE
      result.f := var1 / var2;
      df1 := FLOAT(NUMBER(data1) - 1, R.T);
      df2 := FLOAT(NUMBER(data2) - 1, R.T);
    END;
    result.prob := R.Two * SF.BetaI(0.5D0 * df2, 0.5D0 * df1,
                                    df2 / (df2 + df1 * result.f));
    IF result.prob > R.One THEN result.prob := R.Two - result.prob; END;
    RETURN result;
  END FTest;

PROCEDURE ChiSqr1 (READONLY bins : ARRAY OF R.T;  (* actual bin counts *)
                   READONLY ebins: ARRAY OF R.T;  (* expected bin counts *)
                   constraints: CARDINAL := 1; ): ChiSqrResult
  RAISES {Arith.Error} =
  (* bins has an integer number of events in each bin, ebins has the
     expected number in each bin (possibly non integer), contraints gives
     the constraint count which reduces the df from the number of bins.
     chsq then is a measure of the difference in the bin-by-bin numbers,
     while prob gives the significance of that measure.  Big chsq means big
     difference, big prob means big chance this large chsq came from pure
     random events. *)
  <* UNUSED *>
  CONST
    ftn = Module & "ChiSqr1";

  VAR chsq := R.Zero;
  BEGIN
    <* ASSERT NUMBER(bins) = NUMBER(ebins),
                "bins and ebins have different size" *>

    FOR i := FIRST(bins) TO LAST(bins) DO
      IF bins[i] < 5.0D0 OR ebins[i] < 5.0D0 THEN
        RAISE Arith.Error(NEW(Arith.ErrorNeedMoreData).init());
      END;
      WITH tmp = bins[i] - ebins[i] DO
        chsq := chsq + tmp * tmp / ebins[i];
      END;
    END;

    WITH df = FLOAT(NUMBER(bins) - constraints, R.T) DO
      RETURN ChiSqrResult{chsq := chsq, df := df, prob :=
                          SF.GammaQ(0.5D0 * df, 0.5D0 * chsq)};
    END;
  END ChiSqr1;


PROCEDURE ChiSqr2 (READONLY bins1: ARRAY OF R.T;  (* actual bin1 counts *)
                   READONLY bins2: ARRAY OF R.T;  (* actual bin2 counts *)
                   constraints: CARDINAL := 1; ): ChiSqrResult
  RAISES {Arith.Error} =
  (* bins1 and bins2 have an integer number of events in each bin,
     contraints gives the constraint count which reduces the df from the
     number of bins.  chsq then is a measure of the difference in the
     bin-by-bin numbers, while prob gives the significance of that measure.
     Big chsq means big difference, big prob means big chance this large
     chsq came from pure random events. *)
  <* UNUSED *>
  CONST
    ftn = Module & "ChiSqr2";

  VAR chsq := R.Zero;
  BEGIN
    <* ASSERT NUMBER(bins1) = NUMBER(bins2),
                "bins1 and bins1 have different size" *>

    FOR i := FIRST(bins1) TO LAST(bins1) DO
      IF bins1[i] < 5.0D0 OR bins2[i] < 5.0D0 THEN
        RAISE Arith.Error(NEW(Arith.ErrorNeedMoreData).init());
      END;
      WITH tmp = bins1[i] - bins2[i] DO
        chsq := chsq + tmp * tmp / (bins1[i] + bins2[i]);
      END;
    END;

    WITH df = FLOAT(NUMBER(bins1) - constraints, R.T) DO
      RETURN ChiSqrResult{chsq := chsq, df := df, prob :=
                          SF.GammaQ(R.Half * df, R.Half * chsq)};
    END;
  END ChiSqr2;


BEGIN
END Statistic.
