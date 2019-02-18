(* $Id$ *)

INTERFACE IntSetBits;
IMPORT IntSet;

TYPE
  T <: Public;

  Public = IntSet.T OBJECT METHODS
    init(minValHint := 0; maxValHint := 1) : T;
  END;

CONST Brand = "IntSetBits";

END IntSetBits.
    
