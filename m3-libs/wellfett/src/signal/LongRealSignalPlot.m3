MODULE LongRealSignalPlot;

IMPORT LongRealSignal          AS Signal,
       LongRealPLPlotFigure    AS Figure,
       LongRealPLPlotLineStyle AS LineStyle;

REVEAL T = Public BRANDED OBJECT OVERRIDES init := Init; END;

PROCEDURE Init
  (SELF: T; x: Signal.T; color: CARDINAL; style: LineStyle.T; ): T =
  BEGIN
    RETURN Figure.Lines.initEquidistant(
             SELF, x.getData(), FLOAT(x.getFirst(), LONGREAL),
             color := color, style := style);
  END Init;


BEGIN
END LongRealSignalPlot.
