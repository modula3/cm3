(* $Id$ *)

INTERFACE LRP;
IMPORT LongRealPair;

(* all that this interface accomplishes is that it adds a dummy 
   Compare PROCEDURE to LongRealPair.  Needed for SXType() *)

TYPE T = LongRealPair.T;

CONST Compare : PROCEDURE(READONLY a, b : T) : [-1..1] = NIL;

CONST Equal = LongRealPair.Equal;

CONST Brand = LongRealPair.Brand;

END LRP.
