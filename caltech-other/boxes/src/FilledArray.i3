(* $Id$ *)
INTERFACE FilledArray;
IMPORT IntPairRouteIDTbl AS PairIDTbl;
IMPORT IntPair;

TYPE 
  T <: Public;

  Public = PairIDTbl.T OBJECT METHODS 
    init(defaultIsUnfilled := TRUE) : T;
    haveDataAt(READONLY p : IntPair.T) : BOOLEAN;
  END;

END FilledArray.
  
