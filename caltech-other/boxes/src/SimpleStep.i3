(* $Id$ *)

INTERFACE SimpleStep;
IMPORT GridPoint;

TYPE 
  T = RECORD 
    p : GridPoint.T; 
    prev : GridPoint.T;
    cost : INTEGER;      (* cost of this ONE step *)
  END;

(* if res is NIL or too small, it will be extended ... *)
PROCEDURE Neighbors(READONLY p, 
                    dst : GridPoint.T; 
                    VAR res : Iterator);


TYPE
  Iterator <: PublicIter;

  PublicIter = OBJECT METHODS
    next(VAR s : T) : BOOLEAN
  END;
    
CONST Brand = "Step";

END SimpleStep.
