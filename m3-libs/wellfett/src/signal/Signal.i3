
INTERFACE Signal;

TYPE
  IndexType = INTEGER;
  ElemType  = REAL;

  T <: Public;
  Public = OBJECT
	METHODS
	init(first, last : IndexType);

	getfirst()  : IndexType;
	getlast()   : IndexType;
	getnumber() : IndexType;

	translate  (dist : IndexType);

    convolve   (READONLY with : T) : REF T;
	upsample   (factor : IndexType) : REF T;
	downsample (factor : IndexType) : REF T;
  END;


END Signal.
