MODULE TestTransform;

IMPORT LongRealBasic AS R;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorFast AS VFs;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalVector AS SV;
IMPORT LongRealSignalIntegerPower AS SIntPow;
IMPORT LongRealDiscreteWaveletTransform AS DWT;
IMPORT LongRealDyadicFilterBank AS FB;

IMPORT LongRealBSplineWavelet AS BSpl;

(*IMPORT LongRealFmtLex AS RF;*)
IMPORT LongRealSignalFmtLex AS SF;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;

IMPORT NADefinitions AS NA;

FROM TestMatchWavelet IMPORT TestMatchPatternSmooth;


(*Modifications of the DWT that downsample the lowpass channel only.*)
PROCEDURE DyadicFilterBankAnalysis (         x        : S.T;
                                    READONLY y        : FB.T;
                                             numLevels: CARDINAL; ):
  DWT.DyadicWaveletCoeffs =
  VAR z := NEW(SV.T, numLevels);
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      z[i] := x.convolve(y[1]);
      x := x.convolveDown(y[0], 2);
    END;
    RETURN DWT.DyadicWaveletCoeffs{low := x, high := z};
  END DyadicFilterBankAnalysis;

PROCEDURE DyadicFilterBankSynthesis (READONLY x: DWT.DyadicWaveletCoeffs;
                                     READONLY y: FB.T;
                                     van1: CARDINAL; ): S.T =
  VAR
    z       := x.low;
    vanAtom := NEW(S.T).fromArray(ARRAY OF R.T{R.Half, -R.Half});
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      VAR xVan := SIntPow.MulPower(x.high[i], vanAtom, van1);
      BEGIN
        z := y[0].upConvolve(z, 2).superpose(
               xVan.extractPeaks(2).convolve(y[1]));
        (*extractPeaks works like downsample and subsequent upsample*)
      END;
    END;
    RETURN z;
  END DyadicFilterBankSynthesis;


PROCEDURE PlotSWT (         x        : S.T;
                   READONLY bank     : ARRAY [0 .. 1] OF FB.T;
                            numLevels: CARDINAL;               ) =
  VAR
    unit := IIntPow.MulPower(1, 2, numLevels);
    grid := R.One / FLOAT(unit, R.T);

  (*role of primal and dual filters is swapped here*)
  VAR
    wt    := DWT.DyadicFilterBankAnalysisTI(x, bank[1], numLevels);
    xrec  := DWT.DyadicFilterBankSynthesisTI(wt, bank[0]);
    left  := FLOAT(xrec.getFirst(), R.T) * grid;
    right := FLOAT(xrec.getLast(), R.T) * grid;

  PROCEDURE PlotBand (x: S.T; ) =
    <*FATAL NA.Error*>(*Number of filters and channels will always match*)
    BEGIN
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(
        left, right, VFs.Min(x.getData()^), VFs.Max(x.getData()^));
      PL.SetFGColorDiscr(2);
      PL.PlotLines(
        V.ArithSeq(x.getNumber(), FLOAT(x.getFirst(), R.T) * grid, grid)^,
        x.getData()^);
    END PlotBand;

  BEGIN
    PL.Init();
    PL.SetSubWindows(1, numLevels + 2);
    FOR i := 0 TO numLevels - 1 DO PlotBand(wt.high[i]); END;
    PlotBand(wt.low);
    PlotBand(xrec);
    PL.Exit();
  END PlotSWT;

PROCEDURE PlotDWT (         x        : S.T;
                   READONLY bank     : ARRAY [0 .. 1] OF FB.T;
                            van1     : CARDINAL;
                            numLevels: CARDINAL;               ) =
  VAR
    unit     := IIntPow.MulPower(1, 2, numLevels);
    fineGrid := R.One / FLOAT(unit, R.T);
    grid     := fineGrid;

  (*role of primal and dual filters is swapped here*)
  VAR
    wt    := DyadicFilterBankAnalysis(x, bank[1], numLevels);
    xrec  := DyadicFilterBankSynthesis(wt, bank[0], van1);
    left  := FLOAT(xrec.getFirst(), R.T) * grid;
    right := FLOAT(xrec.getLast(), R.T) * grid;

  PROCEDURE PlotBand (x: S.T; grid: R.T; ) =
    <*FATAL NA.Error*>(*Number of filters and channels will always match*)
    BEGIN
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(
        left, right, VFs.Min(x.getData()^), VFs.Max(x.getData()^));
      PL.SetFGColorDiscr(2);
      PL.PlotLines(
        V.ArithSeq(x.getNumber(), FLOAT(x.getFirst(), R.T) * grid, grid)^,
        x.getData()^);
    END PlotBand;

  BEGIN
    PL.Init();
    PL.SetSubWindows(1, numLevels + 2);
    FOR i := 0 TO numLevels - 1 DO
      grid := grid * R.Two;
      PlotBand(wt.high[i], grid);
    END;
    PlotBand(wt.low, grid);
    PlotBand(xrec, fineGrid);
    PL.Exit();
  END PlotDWT;


PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  BEGIN
    CASE 1 OF
    | 0 =>
        VAR
          dual := FB.T{BSpl.GeneratorMask(2), BSpl.WaveletMask(2, 2)};
          primal := FB.DualToPrimal(
                      ARRAY OF
                        S.T{dual[0].scale(R.Two), dual[1].scale(R.Two)});
        BEGIN
          PlotSWT(S.One, ARRAY OF FB.T{primal, dual}, 6);
        END;
    | 1 =>
        CONST
          numLevels = 6;
          unit      = 64;
          size      = 8 * unit;
          van0      = 1;
          van1      = 6;
        VAR
          bank := TestMatchPatternSmooth(
                    NEW(S.T).fromArray(
                      V.ArithSeq(size, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
                      -size DIV 2), numLevels, 3, van0 + van1, van0, 7,
                    1.0D-10);
          (* Note that the Dual filters are used for Analysis! *)
          reconBank := ARRAY [0 .. 1] OF
                         FB.T{bank[0], FB.T{bank[1, 0].scale(R.Two),
                                            bank[1, 1].scale(R.Two)}};
        <*FATAL Thread.Alerted, Wr.Failure*>
        BEGIN
          IO.Put(
            Fmt.FN(
              "Reconstruction:\n%s\n%s\n",
              ARRAY OF
                TEXT{SF.Fmt(reconBank[0, 0].convolve(reconBank[1, 0])),
                     SF.Fmt(reconBank[0, 1].convolve(reconBank[1, 1]))}));
          PlotDWT(S.One, reconBank, van1, 4);
        END;
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestTransform.
