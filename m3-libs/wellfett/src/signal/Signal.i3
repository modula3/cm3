
INTERFACE Signal;

TYPE
  IndexType = INTEGER;
  ElemType  = REAL;

  T <: Public;
  Public = OBJECT
	METHODS
	init(first, last : IndexType);
	copy() : REF T;

	getfirst()  : IndexType;
	getlast()   : IndexType;
	getnumber() : IndexType;

	translate  (dist   : IndexType);
	upsample   (factor : IndexType) : REF T;
	downsample (factor : IndexType) : REF T;
	wrapcyclic (length : IndexType) : REF T;

    scale      (factor : ElemType);
    raise      (offset : ElemType);

    convolve   (READONLY with : T) : REF T;
    superpose  (READONLY with : T) : REF T;
  END;


END Signal.
