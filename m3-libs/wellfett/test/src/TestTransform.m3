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

PROCEDURE PlotDWT (x: S.T; READONLY filter: ARRAY [0 .. 1] OF S.T; ) =
  CONST
    numlevels = 6;
    unit      = 64;
    grid      = R.One / FLOAT(unit, R.T);

  VAR
    wt := DWT.DyadicFilterBankAnalysisTI(x, filter, numlevels);
    xrec := DWT.DyadicFilterBankSynthesisTI(
              wt, FB.DualToPrimal(
                    ARRAY OF
                      S.T{filter[0].scale(R.Two), filter[1].scale(R.Two)}));
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
    PL.SetSubWindows(1, numlevels + 2);
    FOR i := 0 TO numlevels - 1 DO PlotBand(wt.high[i]); END;
    PlotBand(wt.low);
    PlotBand(xrec);
    PL.Exit();
  END PlotDWT;


PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  BEGIN
    CASE 0 OF
    | 0 =>
        PlotDWT(
          S.One,
          ARRAY OF S.T{BSpl.GeneratorMask(2), BSpl.WaveletMask(2, 2)});
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestTransform.
