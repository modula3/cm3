
GENERIC INTERFACE ContinuousWaveletAnalysis(R, RV, S, Conv, CWT);

FROM CWT IMPORT Wavelet, Width;

CONST Brand = "ContinuousWaveletAnalysis";


(* Continuous wavelet analysis transform using an arbitrary wavelet
   function. *)

TYPE
  (* Abstract handle for an CWT analysis. *)
  T <: REFANY;


PROCEDURE Do (         x      : S.T;
                       wavelet: Wavelet;
                       width  : Width;
              READONLY scales : RV.T;
                       conv   : Conv.Handle := NIL; ): CWT.T;
(* Compute the continuous wavelet transform of the signal 'x' with respect
   to the 'wavelet' for all requested 'scales'.  The wavelet function is
   discretized at 'width' nodes centered around time 0.  The wavelet
   transform is 'width-1' longer than 'x'.  It is convenient to create the
   list of scales using 'Vector.ArithSeq' or 'Vector.GeomSeq'.  'conv' must
   be a freshly created object of a sub-class of 'Conv.Handle'. *)

PROCEDURE New (x      : S.T;
               wavelet: Wavelet;
               width  : Width;
               conv   : Conv.Handle := NIL; ): T;
(* Initialize a continuous wavelet transform for signal 'x' with respect to
   'wavelet'.  Splitting the transform into initialization and execution
   saves memory when the data of each scale is not stored somewhere. *)

PROCEDURE GetScale (h: T; scale: R.T; ): S.T;
(* Compute the continuous wavelet transform for one scale. *)




(* Continuous wavelet transform using a discretized wavelet function. *)

TYPE TDW <: REFANY;

PROCEDURE DoDW (x, wavelet: S.T; READONLY scales: RV.T; ): CWT.T;
(* Compute the continuous wavelet transform of the signal 'x' with respect
   to the 'wavelet' for all requested 'scales'.  The wavelet function must
   be discretized.  It is convenient to create the list of scales using
   'Vector.ArithSeq' or 'Vector.GeomSeq'. *)


PROCEDURE NewDW (x, wavelet: S.T; maxScale: R.T; ): TDW;
(* Initialize a continuous wavelet transform for signal 'x' where the scale
   won't exceed 'maxScale'.  The generated handle can be used to compute
   particular scales of the wavelet transform.  Splitting the transform
   into initialization and execution saves memory when the data of each
   scale is not stored somewhere. *)

PROCEDURE GetScaleDW (h: TDW; scale: R.T; ): S.T;
(* Compute the continuous wavelet transform for one scale. *)


END ContinuousWaveletAnalysis.
