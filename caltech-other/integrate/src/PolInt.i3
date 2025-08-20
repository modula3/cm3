INTERFACE PolInt;

(* polynomial (Lagrange) interpolation, Numerical Recipes in F77, 3.1 

   Neville's method
*)

TYPE Result = RECORD y, dy : LONGREAL END;
     
PROCEDURE Interpolate(READONLY xa, ya : ARRAY OF LONGREAL;
                      x               : LONGREAL) : Result;

CONST Brand = "PolInt";

END PolInt.
