(* $Id: Powell.i3,v 1.2 2001/10/10 07:39:55 mika Exp $ *)

INTERFACE Powell;
IMPORT Matrix;
IMPORT LRScalarField;
IMPORT LRVector;

PROCEDURE Minimize(VAR p  : LRVector.T;
                   VAR xi : Matrix.T;
                   ftol   : LONGREAL;
                   func   : LRScalarField.T) : LONGREAL;

END Powell.
