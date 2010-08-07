(*---------------------------------------------------------------------------*)
INTERFACE OpResultCache;

IMPORT TextSeq;

TYPE 
  CacheIF = BRANDED "OpResultCacheIF 1.0 " OBJECT
  METHODS
    init() : T;
    contains(op, arg : TEXT) : BOOLEAN;
    getText(op, arg : TEXT) : TEXT;
    putText(op, arg, res : TEXT);
    getSeq(op, arg : TEXT) : TextSeq.T;
    putSeq(op, arg : TEXT; res : TextSeq.T);
  END;

  T <: CacheIF;

(*---------------------------------------------------------------------------*)
PROCEDURE New() : T;

END OpResultCache.
