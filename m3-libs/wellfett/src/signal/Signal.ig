
GENERIC INTERFACE Signal(R);

TYPE
  IndexType = INTEGER;

  T <: TPublic;
  TPublic = OBJECT
	METHODS
	init(first, last : IndexType);
	fromArray(READONLY arr : ARRAY OF R.T; first : IndexType := 0);
	copy() : T;

	getfirst()  : IndexType;
	getlast()   : IndexType;
	getnumber() : IndexType;

	translate  (dist   : IndexType);
	upsample   (factor : IndexType) : T;
	downsample (factor : IndexType) : T;
	wrapcyclic (length : IndexType) : T;
	reverse    () : T;
	adjungate  () : T;

    scale      (factor : R.T);
    raise      (offset : R.T);

    convolve   (with : T) : T;
    superpose  (with : T) : T;
  END;


END Signal.
