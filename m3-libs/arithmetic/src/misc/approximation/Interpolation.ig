GENERIC INTERFACE Interpolation(V,R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to interpolation
          routines

          All interpolation routines in this module
          interpolate by computing linear combinations of the node values.
          Thus each linear space can be used for values,
          e.g. complex number, vector, matrix, polynomial

2/17/96  Harry George    Converted to ADT format
*)
FROM NADefinitions IMPORT Error;

(*==========================*)
(*
  We use separate arrays for x and y values,
  this simplifies interpolation of the reverse function.
  xa must be in ascending order
*)

PROCEDURE Linear(
                 READONLY xa:ARRAY OF R.T;     (*interpolation nodes*)
                 READONLY ya:ARRAY OF V.T;     (*interpolation values*)
                 x:R.T;                        (*the function argument*)
                 ):V.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
do linear interpolation for x.
*)

PROCEDURE Newton(
                 READONLY xa:ARRAY OF R.T;     (*interpolation nodes*)
                 READONLY ya:ARRAY OF V.T;     (*interpolation values*)
                 x:R.T;                        (*the function argument*)
                 VAR dy:V.T;                   (*the error estimate*)
                 ):V.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
do Newton polynomial interpolation for x.  Report dy as error estimate.
Partial access: Give the starting index and the length to be used.
*)

PROCEDURE CubicHermite(
                 READONLY xa:ARRAY OF R.T;     (*interpolation nodes*)
                 READONLY ya:ARRAY OF V.T;     (*interpolation values*)
                 x:R.T;                        (*the function argument*)
                 ):V.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
approximate each piece with a cubic polynomial
such that the function is still differentiable at the nodes.
*)

(*==========================*)
END Interpolation.
