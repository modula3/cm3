GENERIC INTERFACE Interpolation(R, V);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Interpolation routines

   All interpolation routines in this module interpolate by computing
   linear combinations of the node values.  Thus each linear space can be
   used for values, e.g.  complex number, vector, matrix, polynomial *)

FROM Arithmetic IMPORT Error;

(*==========================*)
(*
  We use separate arrays for x and y values,
  this simplifies interpolation of the reverse function.
  xa must be in ascending order
*)

PROCEDURE Linear (READONLY xa: ARRAY OF R.T;  (*interpolation nodes*)
                  READONLY ya: ARRAY OF V.T;  (*interpolation values*)
                           x : R.T;           (*the function argument*)
  ): V.T;
(* Given an interpolation table with xa argument and ya function value, do
   linear interpolation for x. *)

PROCEDURE Newton (READONLY xa: ARRAY OF R.T;  (*interpolation nodes*)
                  READONLY ya: ARRAY OF V.T;  (*interpolation values*)
                           x : R.T;           (*the function argument*)
                  VAR      dy: V.T;           (*the error estimate*)
  ): V.T RAISES {Error};
(* Given an interpolation table with xa argument and ya function value, do
   Newton polynomial interpolation for x.  Report dy as error estimate.

   If two nodes (almost) coincide this is reported as
   Arith.ErrorDivisionByZero.  One can also consider this as wrong input
   and make only an ASSERT. *)

PROCEDURE CubicHermite (READONLY xa: ARRAY OF R.T;  (*interpolation nodes*)
                        READONLY ya: ARRAY OF V.T;  (*interpolation
                                                       values*)
                        x: R.T;  (*the function argument*)
  ): V.T;
(* Given an interpolation table with xa argument and ya function value,
   approximate each piece with a cubic polynomial such that the function is
   still differentiable at the nodes. *)

(*==========================*)
END Interpolation.
