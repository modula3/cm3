INTERFACE LongRealWaveletPlot;

IMPORT LongRealSignal AS S;
IMPORT LongRealBasic AS R;

PROCEDURE PlotWavelets (hdual, gdual: S.T; levels: CARDINAL);

PROCEDURE PlotWaveletsYLim (hdual, gdual: S.T;
                            levels      : CARDINAL;
                            ymin, ymax  : R.T       );

END LongRealWaveletPlot.
