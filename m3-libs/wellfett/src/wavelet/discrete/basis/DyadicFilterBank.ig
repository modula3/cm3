GENERIC INTERFACE DyadicFilterBank(S);

TYPE
  T = REF TBody;
  TBody = ARRAY [0 .. 1] OF S.T;


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
