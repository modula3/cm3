GENERIC INTERFACE Interpolation(R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to interpolation
          routines

2/17/96  Harry George    Converted to ADT format
*)
FROM xUtils IMPORT Error;

(*==========================*)
(*
  We use separate arrays for x and y values,
  this simplifies interpolation of the reverse function.
*)

PROCEDURE Linear(
                 READONLY xa,ya:ARRAY OF R.T;  (*interp table*)
                 x:R.T;                        (*the input*)
                 ):R.T  RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do linear interpolation for x.
*)

PROCEDURE Newton(
                 READONLY xa,ya:ARRAY OF R.T;(*interp table*)
                 x:R.T;                      (*the input*)
                 VAR dy:R.T;                 (*the error estimate*)
                 start,len:CARDINAL:=0          (*for partial access*)
                 ):R.T RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do Newton polynomial interpolation for x.  Report dy as error estimate.
Partial access: Give the starting index and the length to be used.
*)

(*==========================*)
END Interpolation.
