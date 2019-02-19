INTERFACE NewUOAs;
IMPORT LRVector, LRScalarField;
IMPORT LongRealSeq AS LRSeq;

PROCEDURE Minimize(p              : LRVector.T;
                   func           : LRScalarField.T;
                   rhobeg, rhoend : LONGREAL;
                   ftarget     := FIRST(LONGREAL)) : Output;

TYPE
  Output = RECORD
    iterations : CARDINAL;
    funcCount  : CARDINAL;
    fhist      : LRSeq.T;
    message    : TEXT;
    f          : LONGREAL;   (* best value found *)
    x          : LRVector.T; (* coords of above *)
  END;
  
END NewUOAs.
