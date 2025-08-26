(* $Id: Compress.i3,v 1.5 2001/10/10 07:39:55 mika Exp $ *)

INTERFACE Compress;
IMPORT LRVector;
IMPORT LRScalarField;

(* Minimize a multivariate function func along the direction xi starting from
   the point p.

   p will be updated to the minimum along the direction.
   xi will be updated to the amount moved along the direction.
   LinMin returns the minimum value found.
*)

CONST Tol = 2.0d-8;

PROCEDURE LinMin(p    : LRVector.T; (* initial and final point *)
                 xi   : LRVector.T; (* search direction, 
                                            replaced with change in p *)
                 func : LRScalarField.T;
                                     
                 scale := 1.0d0;
                 tol   := Tol) : LONGREAL (* returns min. value *);

END Compress.
