
GENERIC INTERFACE ContinuousWaveletSynthesis(R, RV, S, Conv, CWT);

FROM CWT IMPORT Wavelet, Width;

CONST Brand = "ContinuousWaveletSynthesis";


TYPE
  (* Abstract handle for an CWT synthesis. *)
  T <: REFANY;

(* Discretized continuous wavelet synthesis transform.  The algorithm is
   based on discrete convolutions and can provide only an approximate
   reconstruction depending on the wavelet and the scales used. *)

PROCEDURE Do (READONLY w      : CWT.TBody;
                       wavelet: Wavelet;
                       width  : Width;
              READONLY scales : RV.T;
                       conv   : Conv.Handle := NIL; ): S.T;
(* Compute the continuous wavelet transform of the wavelet representation
   'w' with respect to the 'wavelet' for all requested 'scales'.  The
   wavelet function is discretized at 'width' nodes centered around time 0.
   All scales must have contain the same number of wavelet coefficients.
   The restored signal is 'width-1' longer than 'x'.  It is convenient to
   create the list of scales using 'Vector.ArithSeq' or 'Vector.GeomSeq'.
   'conv' must be a freshly created object of a sub-class of
   'Conv.Handle'. *)

PROCEDURE New (wavelet: Wavelet; width: Width; conv: Conv.Handle := NIL; ):
  T;
(* Initialize a continuous wavelet synthesis with respect to 'wavelet'.
   Splitting the transform into initialization and execution saves memory
   if the data of scales is not stored simultaneously. *)

PROCEDURE PutScale (h: T; w: S.T; scale, amp: R.T; );
(* Process another scale of the transform. *)

PROCEDURE Finish (h: T; ): S.T;
(* After all scales are processed, retrieve the restored signal. *)



END ContinuousWaveletSynthesis.
