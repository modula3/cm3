INTERFACE LongRealWaveletPlot;

IMPORT LongRealSignal AS S;
IMPORT LongRealBasic AS R;

PROCEDURE PlotOrthogonal (h: S.T; numlevels: CARDINAL);

PROCEDURE PlotBiorthogonal (hdual, gdual: S.T; numlevels: CARDINAL);

PROCEDURE PlotBiorthogonalYLim (hdual, gdual: S.T;
                                numlevels   : CARDINAL;
                                ymin, ymax  : R.T;      );

PROCEDURE PlotAny (hprimal, gprimal, hdual, gdual: S.T; numlevels: CARDINAL);

END LongRealWaveletPlot.
