
GENERIC INTERFACE ContinuousWaveletTransform(R, RV, C, S, Conv);

CONST Brand = "ContinuousWaveletTransform";

TYPE
  TBody = ARRAY OF S.T;
  T = REF TBody;


(* Continuous wavelet transform using an arbitrary wavelet function. *)

TYPE
  Wavelet = PROCEDURE (x: R.T; ): C.T;
  Width = [1 .. LAST(CARDINAL)];

  (* Abstract handle for an CWT analysis. *)
  Analysis <: REFANY;


PROCEDURE Analyse (         x      : S.T;
                            wavelet: Wavelet;
                            width  : Width;
                   READONLY scales : RV.T;
                            conv   : Conv.Handle := NIL; ): T;
(* Compute the continuous wavelet transform of the signal 'x' with respect
   to the 'wavelet' for all requested 'scales'.  The wavelet function is
   discretized at 'width' nodes centered around time 0.  The wavelet
   transform is 'width-1' longer than 'x'.  It is convenient to create the
   list of scales using 'Vector.ArithSeq' or 'Vector.GeomSeq'.  'conv' must
   be a freshly created object of a sub-class of 'Conv.Handle'. *)

PROCEDURE NewAnalysis (x      : S.T;
                       wavelet: Wavelet;
                       width  : Width;
                       conv   : Conv.Handle := NIL; ): Analysis;
(* Initialize a continuous wavelet transform for signal 'x' with respect to
   'wavelet'.  Splitting the transform into initialization and execution
   saves memory when the data of each scale is not stored somewhere. *)

PROCEDURE AnalysisScale (h: Analysis; scale: R.T; ): S.T;
(* Compute the continuous wavelet transform for one scale. *)




(* Continuous wavelet transform using a discretized wavelet function. *)

TYPE AnalysisDWHandle <: REFANY;

PROCEDURE AnalysisDW (x, wavelet: S.T; READONLY scales: RV.T; ): T;
(* Compute the continuous wavelet transform of the signal 'x' with respect
   to the 'wavelet' for all requested 'scales'.  The wavelet function must
   be discretized.  It is convenient to create the list of scales using
   'Vector.ArithSeq' or 'Vector.GeomSeq'. *)


PROCEDURE AnalysisDWInit (x, wavelet: S.T; maxScale: R.T; ):
  AnalysisDWHandle;
(* Initialize a continuous wavelet transform for signal 'x' where the scale
   won't exceed 'maxScale'.  The generated handle can be used to compute
   particular scales of the wavelet transform.  Splitting the transform
   into initialization and execution saves memory when the data of each
   scale is not stored somewhere. *)

PROCEDURE AnalysisDWScale (h: AnalysisDWHandle; scale: R.T; ): S.T;
(* Compute the continuous wavelet transform for one scale. *)


END ContinuousWaveletTransform.
