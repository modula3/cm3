INTERFACE NewUOA_M3;
IMPORT LRVector, LRScalarField;

PROCEDURE Minimize((*INOUT*)p     : LRVector.T;
                   func           : LRScalarField.T;
                   npt            : CARDINAL;
                   rhobeg, rhoend : LONGREAL;
                   maxfun         : CARDINAL
                   ) : LONGREAL;

END NewUOA_M3.
