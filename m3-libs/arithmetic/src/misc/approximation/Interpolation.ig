GENERIC INTERFACE Interpolation(R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to interpolation
          routines

          The routines should be more generic,
          interpolation can be applied to each kind of vector,
          e.g. complex number, vector, matrix, polynomial

2/17/96  Harry George    Converted to ADT format
*)
FROM xUtils IMPORT Error;

(*==========================*)
(*
  We use separate arrays for x and y values,
  this simplifies interpolation of the reverse function.
  xa must be in ascending order
*)

PROCEDURE Linear(
                 READONLY xa,ya:ARRAY OF R.T;  (*interpolation nodes*)
                 x:R.T;                        (*the function argument*)
                 ):R.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
do linear interpolation for x.
*)

PROCEDURE Newton(
                 READONLY xa,ya:ARRAY OF R.T;  (*interpolation nodes*)
                 x:R.T;                        (*the function argument*)
                 VAR dy:R.T;                   (*the error estimate*)
                 ):R.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
do Newton polynomial interpolation for x.  Report dy as error estimate.
Partial access: Give the starting index and the length to be used.
*)

PROCEDURE CubicHermite(
                 READONLY xa,ya:ARRAY OF R.T;  (*interpolation nodes*)
                 x:R.T;                        (*the function argument*)
                 ):R.T RAISES {Error};
(*Given an interpolation table with xa argument and ya function value,
approximate each piece with a cubic polynomial
such that the function is still differentiable at the nodes.
*)

(*==========================*)
END Interpolation.
