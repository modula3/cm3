GENERIC INTERFACE DaubechiesWavelet(S);

PROCEDURE FilterAbsSqr (n: CARDINAL): S.T;
(* The wavelet filter convoluted with its adjoint.

   To obtain the filter itself you have to apply spectral factorization. *)

PROCEDURE FilterPureAbsSqr (n: CARDINAL): S.T;
(* The wavelet filter convoluted with its adjoint without the factor
   (1,1)^m that corresponds to m zeros at -1. *)

END DaubechiesWavelet.
