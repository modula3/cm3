(* $Id$ *)

INTERFACE ConjGradient;
IMPORT LRVector, LRScalarField, LRVectorField;

EXCEPTION TooManyIterations;

PROCEDURE Minimize(VAR p : LRVector.T;
                   ftol : LONGREAL;
                   func : LRScalarField.T;
                   dfunc : LRVectorField.T) : LONGREAL RAISES { TooManyIterations };

END ConjGradient.
