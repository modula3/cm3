INTERFACE LongRealScaledSignalPlot;

IMPORT LongRealScaledSignal    AS Signal,
       LongRealPLPlotFigure    AS Figure,
       LongRealPLPlotLineStyle AS LineStyle;

TYPE
  T <: Public;
  Public = Figure.Lines OBJECT
           METHODS
             init (x    : Signal.T;
                   color: CARDINAL    := 2;
                   style: LineStyle.T := NIL; ): T;
           END;

END LongRealScaledSignalPlot.
