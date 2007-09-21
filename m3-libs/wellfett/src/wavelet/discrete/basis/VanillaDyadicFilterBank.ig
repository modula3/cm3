GENERIC INTERFACE VanillaDyadicFilterBank(S, FB);

(* This is intended for the filters as used in the classical discrete
   wavelet transform, where the analysis filters are used for correlation
   rather than convolution (the difference is the adjoint). *)

TYPE
  T = FB.T;
  TBody = FB.TBody;
  Pair = FB.Pair;


CONST
  CrossLowPassToHighPass: PROCEDURE (lp: S.T): S.T = FB.OrthogonalLowPassToHighPass;
(* Primal lowpass to dual highpass or dual lowpass to primal highpass. *)

CONST
  CrossHighPassToLowPass: PROCEDURE (hp: S.T): S.T = FB.OrthogonalHighPassToLowPass;
(* Primal highpass to dual lowpass or dual highpass to primal lowpass. *)


PROCEDURE ScaleSqRtTwo (READONLY bank: Pair; ): Pair;
(* scale all filters by sqrt(2) *)

PROCEDURE Reverse (READONLY bank: Pair; ): Pair;
(* reverse all filters *)


END VanillaDyadicFilterBank.
