(* $Id$ *)

INTERFACE LU2_F_sp;
IMPORT RMatrix2 AS M;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V);

END LU2_F_sp.
