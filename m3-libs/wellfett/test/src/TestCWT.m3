MODULE TestCWT;

IMPORT LongRealBasic              AS R,
       LongRealTrans              AS RT,
       LongRealSignal             AS S,
       LongRealVector             AS V,
       LongRealVectorFast         AS VFs,
       LongRealComplexVectorTrans AS CVT;

IMPORT LongRealMatrix AS M;

IMPORT LongRealFmtLex AS RF, LongRealSignalFmtLex AS SF;

IMPORT PLPlot AS PL, PLPlotRaw AS PLRaw;
IMPORT IO, Fmt, Wr, Thread;
IMPORT Arithmetic AS Arith;



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
    BEGIN
      PL.SetFGColorDiscr(1);
      PL.SetEnvironment(xmin, xmax, ymin, ymax);
      PL.PlotShades(
        m^, NIL, xmin, xmax, ymin, ymax, V.ArithSeq(10, -2.0D0, 0.4D0)^, 0,
        0, 0, PLRaw.FillPolygon, TRUE, PL.Plotter0, NIL);
    END;
    PL.Exit();
  END TestShades;

PROCEDURE DiracTransform () =
  BEGIN
  END DiracTransform;


PROCEDURE Test () =
  BEGIN
    CASE 0 OF
    | 0 => TestShades();
    | 1 => DiracTransform();
    ELSE
      <* ASSERT FALSE *>
    END;
  END Test;


BEGIN
END TestCWT.
