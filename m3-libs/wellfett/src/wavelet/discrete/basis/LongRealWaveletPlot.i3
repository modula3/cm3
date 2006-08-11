INTERFACE LongRealWaveletPlot;

IMPORT LongRealBasic  AS R,
       LongRealVector AS V,
       LongRealMatrix AS M,
       LongRealSignal AS S;

PROCEDURE Orthogonal (lp: S.T; numLevels: CARDINAL; );

PROCEDURE Biorthogonal (lpDual, hpDual: S.T; numLevels: CARDINAL; );

PROCEDURE BiorthogonalYLim
  (lpDual, hpDual: S.T; numLevels: CARDINAL; ymin, ymax: R.T; );

PROCEDURE Any (refnPrimal, refnDual              : S.T;
               lpPrimal, hpPrimal, lpDual, hpDual: S.T;
               numLevels                         : CARDINAL; );

PROCEDURE AnyYLim (refnPrimal, refnDual              : S.T;
                   lpPrimal, hpPrimal, lpDual, hpDual: S.T;
                   numLevels                         : CARDINAL;
                   ymin, ymax                        : R.T;      );


PROCEDURE Frame (READONLY abscissa       : V.TBody;
                 READONLY frame          : M.TBody;
                 READONLY wavelet, target: V.TBody; );
(* Plot a set of discretised functions (each row of 'frame' is a function)
   and two other functions in the same window.  Can be used to visualize
   some functions of a basis or a frame. *)

END LongRealWaveletPlot.
