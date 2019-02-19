(* $Id$ *)

INTERFACE Tridiagonal;
IMPORT Matrix, LRVector;

(* Tridiagonal methods *)

PROCEDURE Reduce(VAR a : Matrix.S; 
                 VAR d, e : LRVector.S);
(* reduce real symmetric a to tridiagonal form by Householder reduction *)
(* a replaced by orthogonal Q effecting transformation,
   d by diagonal elements of tridiagonal,
   e by off-diagonal elements.
   e[0] = 0 *)

PROCEDURE QLi(VAR d, e : LRVector.S;
              VAR z : Matrix.S);
(* QL algorithm with implicit shifts.  Use output of reduce.  
   d returns eigenvalues
   e is destroyed
   z returns eigenvectors, column by column *)

END Tridiagonal.













