INTERFACE LongRealWaveletPlot;

IMPORT LongRealSignal AS S;
IMPORT LongRealBasic AS R;

PROCEDURE PlotOrthogonal (h: S.T; numlevels: CARDINAL; );

PROCEDURE PlotBiorthogonal (hDual, gDual: S.T; numlevels: CARDINAL; );

PROCEDURE PlotBiorthogonalYLim (hDual, gDual: S.T;
                                numlevels   : CARDINAL;
                                ymin, ymax  : R.T;      );

PROCEDURE PlotAny (refnPrimal, refnDual          : S.T;
                   hPrimal, gPrimal, hDual, gDual: S.T;
                   numlevels                     : CARDINAL; );

PROCEDURE PlotAnyYLim (refnPrimal, refnDual          : S.T;
                       hPrimal, gPrimal, hDual, gDual: S.T;
                       numlevels                     : CARDINAL;
                       ymin, ymax                    : R.T;      );

END LongRealWaveletPlot.
