MODULE TestTransform;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;
IMPORT LongRealIntegerPower AS RIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorRep AS VR;
IMPORT LongRealVectorFast AS VFs;
IMPORT LongRealVectorTrans AS VT;

IMPORT LongRealMatrix AS M;
IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealFunctional AS Fn;
IMPORT LongRealFunctionalDeriv2 AS FnD;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalVector AS SV;
IMPORT LongRealSignalIntegerPower AS SIntPow;
IMPORT LongRealDiscreteWaveletTransform AS DWT;
IMPORT LongRealDyadicFilterBank AS FB;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealRefinableSmooth AS RefnSm;
IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
(*IMPORT LongRealComplexVectorFmtLex AS CVF;*)
IMPORT LongRealMatrixFmtLex AS MF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;

IMPORT NADefinitions AS NA;

FROM TestMatchWavelet IMPORT TestMatchPatternSmooth;


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
                                     numVan: CARDINAL; ): S.T =
  VAR
    z := x.low;
    van := SIntPow.MulPower(
             S.One, NEW(S.T).fromArray(ARRAY OF R.T{RT.Half, RT.Half}),
             numVan);
  <*FATAL NA.Error*>(*Number of filters and channels will always match*)
  BEGIN
    FOR i := LAST(x.high^) TO FIRST(x.high^) BY -1 DO
      z := y[0].upConvolve(z, 2).superpose(x.high[i].convolve(y[1]));
    END;
    RETURN z;
  END DyadicFilterBankSynthesis;


PROCEDURE PlotDWT (         x        : S.T;
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
          PlotDWT(S.One, ARRAY OF FB.T{primal, dual}, 6);
        END;
    | 1 =>
        CONST
          numLevels = 6;
          unit      = 64;
          size      = 8 * unit;
          van0      = 1;
          van1      = 2;
        VAR
          vanAtom := NEW(S.T).fromArray(ARRAY OF R.T{R.Half, -R.Half});
          bank := TestMatchPatternSmooth(
                    NEW(S.T).fromArray(
                      V.ArithSeq(size, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
                      -size DIV 2), numLevels, 3, van0 + van1, van0, 7,
                    1.0D-10);
          vanBank := ARRAY [0 .. 1] OF
                       FB.T{FB.T{bank[0, 0], bank[0, 1]},
                            FB.T{bank[1, 0], SIntPow.MulPower(
                                               bank[1, 1], vanAtom, van1)}};
          reconBank := ARRAY [0 .. 1] OF
                         FB.T{
                         FB.T{bank[0, 0],
                              SIntPow.MulPower(bank[0, 1], vanAtom, van1)},
                         FB.T{bank[1, 0], bank[1, 1]}};
        BEGIN
          VAR primal := FB.DualToPrimal(vanBank[1]);
          BEGIN
            IO.Put(
              Fmt.FN("Filter bank should be:\n%s\n%s\n%s\n%s\n",
                     ARRAY OF
                       TEXT{SF.Fmt(vanBank[0, 0]), SF.Fmt(primal[0]),
                            SF.Fmt(vanBank[0, 1]), SF.Fmt(primal[1])}));
          END;
          IO.Put(
            Fmt.FN(
              "Reconstruction:\n%s\n%s\n",
              ARRAY OF
                TEXT{SF.Fmt(reconBank[0, 0].convolve(reconBank[1, 0])),
                     SF.Fmt(reconBank[0, 1].convolve(reconBank[1, 1]))}));
          PlotDWT(S.One, reconBank, 1);
        END;
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestTransform.
