GENERIC INTERFACE DaubechiesWavelet(S);

PROCEDURE Filter (n: CARDINAL): S.T;
(* The generator mask.

   'n' is restricted to the values that 'FilterPure' support. *)

PROCEDURE FilterPure (n: CARDINAL): S.T;
(* The generator mask without the factor (1,1)^m that corresponds to m
   zeros at -1.

   This is currently limited to orders 0, 1, 2, 4.  A general solution
   requires spectral factorization which is still not implemented. *)

PROCEDURE FilterAbsSqr (n: CARDINAL): S.T;
(* The generator mask convoluted with its adjoint. *)

PROCEDURE FilterPureAbsSqr (n: CARDINAL): S.T;
(* The generator mask convoluted with its adjoint without the factor
   (1,1)^m that corresponds to m zeros at -1. *)

END DaubechiesWavelet.
