(* $Id$ *)
INTERFACE Bins;
IMPORT IntPairList, MagRect, IntPairSet;
IMPORT IntPair;

(* just a routine to turn each rect into a bunch of bins that it may touch *)
PROCEDURE Compute(step : CARDINAL; rect : MagRect.T) : IntPairList.T;
PROCEDURE ComputeAndAddToSet(step : CARDINAL; rect : MagRect.T;
                             set : IntPairSet.T);
PROCEDURE ComputeArray(step : CARDINAL; 
                       rect : MagRect.T) : REF ARRAY OF IntPair.T;

(* will use passed-in iterator if non-NIL, else allocate one *)
PROCEDURE ComputeIterator(step : CARDINAL; rect : MagRect.T;
                          VAR iterator : Iterator);

TYPE 
  Iterator <: PublicIterator;

  PublicIterator = MUTEX OBJECT METHODS
    next(VAR pair : IntPair.T) : BOOLEAN
  END;


END Bins.
