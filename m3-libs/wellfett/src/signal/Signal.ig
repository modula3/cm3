
GENERIC INTERFACE Signal(R);

TYPE
  IndexType = INTEGER;

  T <: TPublic;
  TPublic = OBJECT
	METHODS
	init(first, last : IndexType);
	fromArray(READONLY arr : ARRAY OF R.T; first : IndexType := 0);
	copy() : T;

    (*simply 'first' would collide with a instance variable*)
	getFirst()  : IndexType;
	getLast()   : IndexType;
	getNumber() : IndexType;

	translate  (dist   : IndexType);
	upsample   (factor : IndexType) : T;
	downsample (factor : IndexType) : T;
	wrapCyclic (length : IndexType) : T;
	reverse    () : T;
	adjungate  () : T;

    scale      (factor : R.T);
    raise      (offset : R.T);

    convolve   (with : T) : T;
    superpose  (with : T) : T;
  END;


END Signal.
