
GENERIC INTERFACE ContinuousWaveletTransform(R, C, CV, CS);

CONST Brand = "ContinuousWaveletTransform";

TYPE
  Wavelet = PROCEDURE (x: R.T; ): C.T;
  Width = [1 .. LAST(CARDINAL)];

  TBody = ARRAY OF CS.T;
  T = REF TBody;
(* Each element of the array T represents a scale of the discretized
   continuous wavelet transform. *)


PROCEDURE DiscretizeWavelet (wavelet   : Wavelet;
                             scale, amp: R.T;
                             width     : Width;   ): CV.T;
(* Helper function for discretizing wavelet for analysis and synthesis. *)


END ContinuousWaveletTransform.
