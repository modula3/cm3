GENERIC INTERFACE Interpolation(R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to interpolation
          routines

2/17/96  Harry George    Converted to ADT format
*)
FROM xUtils IMPORT Error;
FROM xReal64 IMPORT REAL64;

(*==========================*)
PROCEDURE linear(
                 READONLY xa,ya:ARRAY OF REAL64;  (*interp table*)
                 x:REAL64;                        (*the input*)
                 ):REAL64  RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do linear interpolation for x.
*)

PROCEDURE newt(
                 READONLY xa,ya:ARRAY OF REAL64;(*interp table*)
                 x:REAL64;                      (*the input*)
                 VAR dy:REAL64;                 (*the error estimate*)
                 start,len:CARDINAL:=0          (*for partial access*)
                 ):REAL64 RAISES {Error};
(*Given an interpolation table with xa input and ya output,
do Newton polynomial interpolation for x.  Report dy as error estimate.
Partial access: Give the starting index and the length to be used.
*)

(*==========================*)
END Interpolation.
