MODULE TestRandom EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details Abstract: Tests for Rand
   module.

   3/16/96 Harry George Initial version (basic structure)

   3/17/96 Warren Smith Normal, Gamma, and Dirichlet *)

IMPORT LongRealBasic            AS R,
       LongRealFmtLex           AS RF,
       RandomBasic,
       RandomDECSRC             AS DECSRC,
       RandomParkMiller         AS ParkMill,
       RandomBayesDurham        AS BayDur,
       RandomCombinedSlow       AS CombSlow,
       RandomCombinedFast       AS CombFast,
       RandomIteratedSquaring   AS IterSqr,
       RandomSubtractiveFibo1   AS SubFibo1,
       RandomSubtractiveFibo2   AS SubFibo2,
       RandomMultiplicativeFibo AS MulFibo,
       RandomQuaternaryFibo     AS QuaFibo,
       RandomImprovedMcGill     AS McGill,
       RandomWolframCA          AS Wolf;
IMPORT Statistic, Fmt, Arithmetic AS Arith;


CONST Module = "TestRandom.";


PROCEDURE PrintStats (name: TEXT; READONLY data: ARRAY OF R.T; ) =
  CONST fmtStyle = RF.FmtStyle{prec := 6, style := Fmt.Style.Fix};
  VAR r := Statistic.FromData(data);
  BEGIN
    Msg("\n" & name);
    Msg("\n" & " min =" & RF.Fmt(r.min, fmtStyle) & " max ="
          & RF.Fmt(r.max, fmtStyle) & " mean="
          & RF.Fmt(r.avrgVar.avrg, fmtStyle) & "\n" & " sdev="
          & RF.Fmt(r.sdev, fmtStyle) & " var ="
          & RF.Fmt(r.avrgVar.var, fmtStyle) & " skew="
          & RF.Fmt(r.skew, fmtStyle) & " kurt=" & RF.Fmt(r.kurt, fmtStyle)
          & "\n");
  END PrintStats;

PROCEDURE TestEngines (): BOOLEAN =
  CONST
    ftn = Module & "TestEngines";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR
    result   := TRUE;
    decsrc   := NEW(DECSRC.T).init();
    parkmill := NEW(ParkMill.T).init();
    baydur   := NEW(BayDur.T).init();
    combslow := NEW(CombSlow.T).init();
    combfast := NEW(CombFast.T).init();


  PROCEDURE DoEngine (name: TEXT; rand: RandomBasic.T; ) =
    VAR data := NEW(R.Array, N);
    <* FATAL Arith.Error *>
    BEGIN
      FOR i := n1 TO nn DO data[i] := rand.uniform(); END;
      PrintStats(name, data^);
    END DoEngine;


  BEGIN
    Debug(1, ftn, "begin\n");

    Msg("N=" & Fmt.Int(N) & "\n");

    DoEngine("DECSRC  ", decsrc);
    DoEngine("parkmill", parkmill);
    DoEngine("baydur  ", baydur);
    DoEngine("combslow", combslow);
    DoEngine("combfast", combfast);

    RETURN result;
  END TestEngines;

PROCEDURE TestCombined (): BOOLEAN =
  CONST ftn = Module & "TestCombined";
  VAR
    is       := NEW(IterSqr.T).init();
    subfibo1 := NEW(SubFibo1.T).init(is);
    subfibo2 := NEW(SubFibo2.T).init(is);
    mulfibo  := NEW(MulFibo.T).init(is);
    quafibo  := NEW(QuaFibo.T).init(is);
    mcgill   := NEW(McGill.T).init(is);
    wolf     := NEW(Wolf.T).init(is);
    result   := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    (*
      IO.Put( Fmt.LongReal( Uni01() ) & "\n");
    *)

    (*
      FOR i:=0 TO 10000000 DO
    *)
    FOR i := 0 TO 1000000 DO
      EVAL subfibo1.engine();
      EVAL subfibo2.engine();
      EVAL mulfibo.engine();
      EVAL quafibo.engine();
      EVAL mcgill.engine();
      EVAL wolf.engine();
    END;

    RETURN result;
  END TestCombined;

PROCEDURE TestUniform (): BOOLEAN =
  CONST
    ftn = Module & "TestUniform";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR
    result := TRUE;
    rand   := NEW(CombFast.T).init();
    data1  := NEW(R.Array, N);
    data2  := NEW(R.Array, N);
    data3  := NEW(R.Array, N);
  <* FATAL Arith.Error *>
  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := n1 TO nn DO
      data1[i] := rand.uniform();
      data2[i] := rand.uniform(min := -1.0D0, max := +1.0D0);
      data3[i] := rand.uniform(min := +200.0D0, max := +1000.0D0);
    END;
    PrintStats("0..1", data1^);
    PrintStats("-1..+1", data2^);
    PrintStats("200..1000", data3^);

    RETURN result;
  END TestUniform;

PROCEDURE TestExponential (): BOOLEAN =
  CONST
    ftn = Module & "TestExponential";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR
    result := TRUE;
    rand   := NEW(CombFast.T).init();
    data1  := NEW(R.Array, N);
  BEGIN
    Debug(1, ftn, "begin\n");
    FOR i := n1 TO nn DO data1[i] := rand.exponential(); END;
    PrintStats("exponential, mean=1", data1^);

    RETURN result;
  END TestExponential;


PROCEDURE TestNormal (): BOOLEAN =
  CONST
    ftn = Module & "TestNormal";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR
    result := TRUE;
    rand   := NEW(CombFast.T).init();
    data1  := NEW(R.Array, N);
  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := n1 TO nn DO data1[i] := rand.gaussian(); END;
    PrintStats("Normal (Gaussian): mean=0, var=1", data1^);
    RETURN result;
  END TestNormal;

PROCEDURE TestBinomial (): BOOLEAN =
  CONST
    ftn        = Module & "TestBinomial";
    N          = 10000;
    n1         = 0;
    nn         = N - 1;
    numBuckets = 15;
  VAR
    result           := TRUE;
    rand             := NEW(CombFast.T).init();
    data1            := NEW(R.Array, N);
    count            := NEW(REF ARRAY OF CARDINAL, numBuckets + 1);
    curcnt: CARDINAL;
  BEGIN
    Debug(1, ftn, "begin\n");

    FOR j := 0 TO numBuckets DO count[j] := 0; END;

    FOR i := n1 TO nn DO
      curcnt := rand.binomial(0.4D0, numBuckets);
      data1[i] := FLOAT(curcnt, R.T);
      INC(count[curcnt]);
    END;
    PrintStats("Binomial " & Fmt.Int(numBuckets), data1^);

    FOR j := 0 TO numBuckets DO
      Msg(Fmt.FN(
            "%2s: %s\n",
            ARRAY OF TEXT{Fmt.Int(j), Fmt.Pad("", count[j] DIV 100, '-')}));
    END;

    RETURN result;
  END TestBinomial;

PROCEDURE TestGamma (): BOOLEAN =
  CONST
    ftn = Module & "TestGamma";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR
    result := TRUE;
    rand   := NEW(CombFast.T).init();
    data1  := NEW(R.Array, N);
    data2  := NEW(R.Array, N);
    data3  := NEW(R.Array, N);
  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := n1 TO nn DO
      data1[i] := rand.gamma(1.0d0);
      data2[i] := rand.gamma(2.5d0);
      data3[i] := rand.gamma(5.1d0);
    END;
    PrintStats("gamma(1.0)", data1^);
    PrintStats("gamma(2.5)", data2^);
    PrintStats("gamma(5.1)", data3^);
    RETURN result;
  END TestGamma;

PROCEDURE TestDirichlet (): BOOLEAN =
  CONST
    ftn = Module & "TestDirichlet";
    N   = 10000;
    n1  = 0;
    nn  = N - 1;
  VAR result := TRUE;
  (*
    rand:=NEW(CombFast.T).init();
    data1:=NEW(R.Array,N);
    data2:=NEW(R.Array,N);
    data3:=NEW(R.Array,N);
  *)
  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := n1 TO nn DO END;
    RETURN result;
  END TestDirichlet;


PROCEDURE TestRandom (): BOOLEAN =
  <* UNUSED *>
  CONST
    ftn = Module & "TestRandom";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestEngines();
    NewLine();
    EVAL TestCombined();
    NewLine();
    EVAL TestUniform();
    NewLine();
    EVAL TestExponential();
    NewLine();
    EVAL TestNormal();
    NewLine();
    EVAL TestBinomial();
    NewLine();
    EVAL TestGamma();
    NewLine();
    EVAL TestDirichlet();
    RETURN result;
  END TestRandom;

BEGIN
END TestRandom.
