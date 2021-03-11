(* $Id$ *)

INTERFACE Powell;
IMPORT Matrix;
IMPORT LRScalarField;
IMPORT LRVector;

PROCEDURE Minimize(VAR p : LRVector.T;
                   VAR xi : Matrix.T;
                   ftol : LONGREAL;
                   func : LRScalarField.T) : LONGREAL;

END Powell.
