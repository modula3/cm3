GENERIC INTERFACE DyadicFilterBank(S);

(* This is intended for the bare filters as used in the convolution based
   discrete wavelet transform.  That is, when translating from the primal
   to the dual or back, no flip of the filter is performed. *)

TYPE
  T = REF TBody;
  TBody = ARRAY [0 .. 1] OF S.T;
  Pair = ARRAY [0 .. 1] OF TBody;


PROCEDURE PrimalLowPassToDualHighPass (x: S.T; ): S.T;

PROCEDURE PrimalHighPassToDualLowPass (x: S.T; ): S.T;

PROCEDURE DualLowPassToPrimalHighPass (x: S.T; ): S.T;

PROCEDURE DualHighPassToPrimalLowPass (x: S.T; ): S.T;


PROCEDURE PrimalToDual (READONLY x: TBody; ): TBody;
(* Convert the primal part of a biorthogonal filter bank to the dual
   one. *)

PROCEDURE DualToPrimal (READONLY x: TBody; ): TBody;
(* Convert the primal part of a biorthogonal filter bank to the dual
   one. *)


PROCEDURE OrthogonalLowPassToHighPass (lp: S.T): S.T;
(* If x is the generator mask of an orthogonal filter bank, compute the
   wavelet mask. *)

PROCEDURE OrthogonalHighPassToLowPass (hp: S.T): S.T;
(* If x is the wavelet mask of an orthogonal filter bank, compute the
   generator mask. *)

END DyadicFilterBank.
