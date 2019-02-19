(* $Id$ *)

INTERFACE Compress;
IMPORT Matrix;
IMPORT LRScalarField;

(* Minimize a multivariate function func along the direction xi starting from
   the point p.

   p will be updated to the minimum along the direction.
   xi will be updated to the amount moved along the direction.
   LinMin returns the minimum value found.
*)

PROCEDURE LinMin(VAR p : Matrix.Vector; (* initial and final point *)
                 VAR xi : Matrix.Vector; (* search direction, 
                                            replaced with change in p *)
                 func : LRScalarField.T) : LONGREAL (* returns min. value *);

END Compress.
