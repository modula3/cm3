
GENERIC INTERFACE Signal(R);

TYPE
  IndexType = INTEGER;

  T <: Public;
  Public = OBJECT
	METHODS
	init(first, last : IndexType);
	copy() : T;

	getfirst()  : IndexType;
	getlast()   : IndexType;
	getnumber() : IndexType;

	translate  (dist   : IndexType);
	upsample   (factor : IndexType) : T;
	downsample (factor : IndexType) : T;
	wrapcyclic (length : IndexType) : T;

    scale      (factor : R.T);
    raise      (offset : R.T);

    convolve   (with : T) : T;
    superpose  (with : T) : T;
  END;


END Signal.
