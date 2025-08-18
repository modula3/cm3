(* $Id: ConjGradient.i3,v 1.3 2001/10/10 07:39:55 mika Exp $ *)

INTERFACE ConjGradient;
IMPORT LRVector, LRScalarField, LRVectorField;

EXCEPTION TooManyIterations;

PROCEDURE Minimize(VAR p : LRVector.T;
                   ftol  : LONGREAL;
                   func  : LRScalarField.T;
                   dfunc : LRVectorField.T) : LONGREAL
  RAISES { TooManyIterations };

END ConjGradient.
