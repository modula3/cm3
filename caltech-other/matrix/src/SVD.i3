(* $Id$ *)

INTERFACE SVD;

IMPORT Matrix;

EXCEPTION NoConvergence;

PROCEDURE Decompose((* INOUT *) a : Matrix.T;
                    (* OUT *) w : Matrix.Vector;
                    (* OUT *) v : Matrix.T) RAISES { NoConvergence };
  (* given a matrix a, 

     perform Singular Value Decomposition

     A = U W VT
     
     A of size M x N

     U of size M x N
     W of size N x N
     V of size N x N

     Where U and V are orthogonal, and W is a diagonal matrix with
     certain properties.

     U replaces A on output.

     The diagonal (only) of W is returned in W (must be sized properly
     on call).  W is, physically, an Nx1 column vector.

     V (not VT) returned in V.

     For efficiency reasons, you have to allocate W and V yourself.

     Numerical recipes in Fortran 77, p. 59
    *)

PROCEDURE BackSubstitute(u : Matrix.T;
                         w : Matrix.Vector;
                         v : Matrix.T;

                         b : Matrix.Vector;
                         (* OUT *) x : Matrix.Vector);
(* compute sol'n x to Ax = b using SVD data from above.
   In order to use it effectively, zero out singular values that
   are too small before calling. *)

END SVD.
