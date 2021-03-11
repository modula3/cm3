(* $Id$ *)
INTERFACE EntryWays;
FROM EndPointStatus IMPORT Dir;
IMPORT MagPointList;
IMPORT RectSet;

TYPE 
  T = ARRAY Dir OF MagPointList.T;

  Tagged = RECORD 
    refTag : REFANY;
    tag, otherRectsInSameNet : RectSet.T; 
    ways : T; 
    defaultLayoutOK : BOOLEAN 
  END;

CONST Brand = "EntryWays";

END EntryWays.
