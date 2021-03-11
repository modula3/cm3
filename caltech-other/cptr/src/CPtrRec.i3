INTERFACE CPtrRec;
IMPORT Ctypes,WeakRef;

CONST
  Brand = "CPtrRec";

TYPE
  T = OBJECT
    c : Ctypes.void_star;
    m : WeakRef.T; 
    refCnt : CARDINAL := 0; (* keeps track of how many surrogates are around *)
  END;

END CPtrRec.
