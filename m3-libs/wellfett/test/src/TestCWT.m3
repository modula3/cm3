MODULE TestCWT;

IMPORT LongRealBasic       AS R,
       LongRealTrans       AS RT,
       LongRealSignal      AS S,
       LongRealVector      AS V,
       LongRealVectorTrans AS VT;

IMPORT LongRealMatrix AS M;

(* Why isn't it reported as unused? *)
IMPORT LongRealFmtLex AS RF, LongRealSignalFmtLex AS SF;

IMPORT LongRealConvolution AS Conv;
IMPORT LongRealContinuousWaveletAnalysis  AS CWA,
       LongRealContinuousWaveletSynthesis AS CWS;

IMPORT PLPlot AS PL, PLPlotRaw AS PLRaw;
IMPORT IO, Fmt;



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
    RETURN RT.SqRt(R.Two / RT.SqRtPi) * t * RT.Exp(-t * t / R.Two);
  END GaussianDiff;

PROCEDURE AssertApproxSignal (x, y: S.T; ) =
  BEGIN
    IO.Put(Fmt.F("metric distance %s, norm2 %s %s\n",
                 RF.Fmt(VT.Norm2Sqr(V.Sub(x.getData(), y.getData()))),
                 RF.Fmt(VT.Norm2Sqr(x.getData())),
                 RF.Fmt(VT.Norm2Sqr(y.getData()))));
    <* ASSERT x.getFirst() = y.getFirst() *>
    <* ASSERT VT.Norm2Sqr(V.Sub(x.getData(), y.getData()))
                < 1.0D0 * RT.Eps * RT.Eps
                    * (VT.Norm2Sqr(x.getData()) + VT.Norm2Sqr(y.getData())) *>
  END AssertApproxSignal;

PROCEDURE DiracTransform () =
  CONST
    width     = 201;
    numScales = 101;

    xmin = -FLOAT(width DIV 2, R.T);
    xmax = FLOAT(width DIV 2, R.T);
    ymin = -FLOAT(numScales DIV 2, R.T);
    ymax = FLOAT(numScales DIV 2, R.T);

  VAR
    scales := V.GeomSeq(numScales, 30.0D0, RT.Pow(R.Half, R.One / 20.0D0));
    yf := CWA.Do(
            S.One, GaussianDiff, width, scales^, NEW(Conv.HandleFourier));
    zf := CWS.Do(
            yf^, GaussianDiff, width, scales^, NEW(Conv.HandleFourier));
    yn := CWA.Do(
            S.One, GaussianDiff, width, scales^, NEW(Conv.HandleNaive));
    zn := CWS.Do(yn^, GaussianDiff, width, scales^, NEW(Conv.HandleNaive));

  BEGIN
    IO.Put(Fmt.F("y.first %s, z.first %s\n", Fmt.Int(yf[0].getFirst()),
                 Fmt.Int(zf.getFirst())));
    <* ASSERT NUMBER(yn^) = NUMBER(yf^) *>
    FOR i := FIRST(yn^) TO LAST(yn^) DO
      AssertApproxSignal(yn[i], yf[i]);
    END;
    AssertApproxSignal(zn, zf);

    PL.Init();
    VAR m := NEW(M.T, numScales, width);
    BEGIN
      PL.SetEnvironment(xmin, xmax, ymin, ymax);
      FOR i := FIRST(m^) TO LAST(m^) DO m[i] := yn[i].getData()^; END;
      PL.PlotImage(M.Transpose(m)^, xmin, xmax, ymin, ymax, -200.0D0,
                   200.0D0, xmin, xmax, ymin, ymax);
      PL.PlotLines(V.ArithSeq(width, xmin, R.One)^,
                   V.Scale(zf.clipToVector(-(width DIV 2), width), 4.0D1)^);
      (* PL.PlotLines(V.ArithSeq(width * 2 - 1, xmin, R.Half)^,
         V.Scale(z.getData(), 5.0D-6)^); *)
    END;
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
