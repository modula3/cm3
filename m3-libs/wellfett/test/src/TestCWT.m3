MODULE TestCWT;

IMPORT LongRealBasic              AS R,
       LongRealTrans              AS RT,
       LongRealSignal             AS S,
       LongRealVector             AS V,
       LongRealVectorFast         AS VFs,
       LongRealComplexVectorTrans AS CVT;

IMPORT LongRealMatrix AS M;

IMPORT LongRealFmtLex AS RF, LongRealSignalFmtLex AS SF;

IMPORT LongRealConvolution AS Conv;
IMPORT LongRealContinuousWaveletTransform AS CWT;

IMPORT PLPlot AS PL, PLPlotRaw AS PLRaw;
IMPORT IO, Fmt, Wr, Thread;



PROCEDURE TestShades () =
  BEGIN
    PL.Init();
    VAR
      xmin := 0.0D0;
      xmax := 5.0D0;
      ymin := 0.0D0;
      ymax := 5.0D0;
      m := M.DiagonalFromArray(
             V.TBody{1.0D0, -1.0D0, 0.0D0, -2.0D0, 2.0D0});
      width  := 5.0D0;
      height := 5.0D0;
    BEGIN
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(xmin, xmax, ymin, ymax);
      PL.PlotShades(
        m^, NIL, xmin, xmax, ymin, ymax, V.ArithSeq(10, -2.5D0, 0.5D0)^, 0,
        0, 0, PLRaw.FillPolygon, TRUE, PL.Plotter0, NIL);
      PL.SetEnvironment(xmin, xmax, ymin, ymax);
      PL.PlotImage(m^, 0.0D0, width, 0.0D0, height, -2.0D0, 2.0D0, 0.0D0,
                   width, 0.0D0, height);
    END;
    PL.Exit();
  END TestShades;

<* UNUSED *>
PROCEDURE MexicanHat (t: R.T; ): R.T =
  BEGIN
    WITH t2 = t * t DO
      RETURN R.Two / RT.SqRt(3.0D0 * RT.SqRtPi) * (R.One - t2) * RT.Exp(
               -t2 / R.Two);
    END;
  END MexicanHat;

PROCEDURE GaussianDiff (t: R.T; ): R.T =
  BEGIN
    RETURN t * RT.Exp(-t * t / R.Two);
  END GaussianDiff;

PROCEDURE DiracTransform () =
  CONST
    width     = 201;
    numScales = 100;

    xmin = 0.0D0;
    xmax = FLOAT(width, R.T);
    ymin = 0.0D0;
    ymax = FLOAT(numScales, R.T);
  VAR
    y := CWT.Analyse(
           S.One, GaussianDiff, width,
           V.GeomSeq(numScales, 30.0D0, RT.Pow(R.Half, R.One / 20.0D0))^,
           NEW(Conv.HandleFourier));
    m := NEW(M.T, numScales, width);
  BEGIN
    PL.Init();
    PL.SetEnvironment(xmin, xmax, ymin, ymax);
    FOR i := FIRST(m^) TO LAST(m^) DO m[i] := y[i].getData()^; END;
    PL.PlotImage(M.Transpose(m)^, xmin, xmax, ymin, ymax, -200.0D0,
                 200.0D0, xmin, xmax, ymin, ymax);
    PL.Exit();
  END DiracTransform;


PROCEDURE Test () =
  BEGIN
    CASE 1 OF
    | 0 => TestShades();
    | 1 => DiracTransform();
    ELSE
      <* ASSERT FALSE *>
    END;
  END Test;


BEGIN
END TestCWT.
