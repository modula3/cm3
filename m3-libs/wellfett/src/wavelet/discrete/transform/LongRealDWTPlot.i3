INTERFACE LongRealDWTPlot;

IMPORT LongRealBasic                          AS R,
       LongRealSignal                         AS S,
       LongRealDyadicDiscreteWaveletTransform AS DWTransform;

IMPORT LongRealScaledSignalSeq AS ScaledSignalSeq,
       LongRealPLPlotFrame     AS Frame,
       LongRealPLPlotLineStyle AS LineStyle;


(* Plot all bands of a discrete wavelet transform.  'y' is an additional
   signal that can be plotted aligned to the transform.  This might be the
   original signal or the reconstructed signal.  'samplePeriod' is e.g.  of
   the time difference between neighboured sample values of the original
   signal. *)

PROCEDURE DWT (wt: DWTransform.T; y: S.T := NIL; samplePeriod := R.One; );

PROCEDURE ShiftInvariantDWT
  (wt: DWTransform.T; y: S.T := NIL; samplePeriod := R.One; );

PROCEDURE HighBandOversampledDWT
  (wt: DWTransform.T; y: S.T := NIL; samplePeriod := R.One; );


(* could be moved to DyadicDiscreteWaveletTransform, but these routines
   return plain sequences rather than a record *)
PROCEDURE DWTToSigSeq (wt: DWTransform.T; samplePeriod := R.One; ):
  ScaledSignalSeq.T;

PROCEDURE ShiftInvariantDWTToSigSeq
  (wt: DWTransform.T; samplePeriod := R.One; ): ScaledSignalSeq.T;

PROCEDURE HighBandOversampledDWTToSigSeq
  (wt: DWTransform.T; samplePeriod := R.One; ): ScaledSignalSeq.T;

PROCEDURE SigSeqToFrameArr (xs        : ScaledSignalSeq.T;
                            color     : CARDINAL            := 2;
                            style     : LineStyle.T         := NIL;
                            frameColor: CARDINAL            := 1;
                            sizeX                           := 1.0D0;
                            sizeY                           := 0.5D0; ):
  REF ARRAY OF Frame.Single;


TYPE
  T <: Public;
  Public = Frame.Matrix OBJECT
           METHODS
             init (xs        : ScaledSignalSeq.T;
                   color     : CARDINAL            := 2;
                   style     : LineStyle.T         := NIL;
                   frameColor: CARDINAL            := 1;
                   sizeX                           := 1.0D0;
                   sizeY                           := 0.5D0; ): T;
           END;


END LongRealDWTPlot.
