
GENERIC INTERFACE DyadicDiscreteWaveletTransform(S, SV, FB);

CONST Brand = "DyadicDiscreteWaveletTransform";

TYPE
  T = RECORD
        low : S.T;
        high: SV.T;
      END;

PROCEDURE FromSignal (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ):
  T;
(* Transform signal x into 'numLevels' frequency bands with octave distance
   and one low frequency band.*)

PROCEDURE ToSignal (READONLY x: T; READONLY y: FB.TBody; ): S.T;
(* Transform sub bands into the original signal.  The reconstructed signal
   will be longer than the original one but the extra values should be zero
   if the filter bank allows for perfect reconstruction.*)

PROCEDURE ShiftInvariantFromSignal
  (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ): T;
(* Translation invariant transform, this is achieved by omitting the
   subsampling. *)

PROCEDURE ShiftInvariantToSignal (READONLY x: T; READONLY y: FB.TBody; ):
  S.T;
(* Translation invariant synthesis.  The routine halves the amplitude on
   each merge of two bands, thus the same filter banks as for the
   subsampling transform can be used. *)



(* Modification of the DWT that downsample the lowpass channel only.  A
   convolutional factor of the analysis highpass filter (e.g.  vanishing
   moment factors) can be deferred to the synthesis transform.  There the
   deferred filter factor is applied, the resulting signal is downsampled
   and upsampled and the synthesis highpass is applied. *)

PROCEDURE HighBandOversampledFromSignal
  (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ): T;

PROCEDURE HighBandOversampledToSignal
  (READONLY x: T; READONLY y: FB.TBody; ): S.T;


PROCEDURE HighBandConvolveDown (x: T; y: S.T; ): T;
(* Convolve each band except of x.low with y and down sample it.  This is
   usually used to post-process HighBandOversampledFromSignal. *)

PROCEDURE HighBandUpConvolve (x: T; y: S.T; ): T;
(* Up sample each band except of x.low and convolve it with y.  This is
   usually used to pre-process HighBandOversampledToSignal. *)

END DyadicDiscreteWaveletTransform.
