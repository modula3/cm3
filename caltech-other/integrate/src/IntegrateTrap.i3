(* $Id$ *)

INTERFACE IntegrateTrap;
IMPORT LRFunction AS Function;

(* integrate things using the trapezoidal rule *)

EXCEPTION NoConvergence;


(* integrate func from a to b in 2^n+1 steps using the trapezoid rule *)
PROCEDURE IntegrateN(func : Function.T; 
                     a , b : LONGREAL;
                     n : CARDINAL) : LONGREAL;


CONST DefaultEps = 1.0d-5;

(* integrate func from a to b until the fractional accuracy eps *)
(* has been reached; abort if more than 2^jmax steps have been taken. *)
PROCEDURE IntegrateE(func : Function.T; 
                     a , b : LONGREAL;
                     eps := DefaultEps;
                     jmax : CARDINAL  := 20) : LONGREAL RAISES { NoConvergence };

PROCEDURE SimpsonE(func : Function.T; 
                   a , b : LONGREAL;
                   eps := DefaultEps;
                   jmax : CARDINAL := 20) : LONGREAL RAISES { NoConvergence };

END IntegrateTrap.
