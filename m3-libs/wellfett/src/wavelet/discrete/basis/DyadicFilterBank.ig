GENERIC INTERFACE DyadicFilterBank(S);

TYPE T = ARRAY [0 .. 1] OF S.T;

PROCEDURE Complement (READONLY x: T): T;
(* If x is the primal or dual part of a biorthogonal filter bank, get the
   dual or primal one, respectively. *)

PROCEDURE OrthogonalWavelet (h: S.T): S.T;
(* If x is the generator mask of an orthogonal filter bank, compute the
   wavelet mask. *)

PROCEDURE OrthogonalGenerator (g: S.T): S.T;
(* If x is the wavelet mask of an orthogonal filter bank, compute the
   generator mask. *)

END DyadicFilterBank.
