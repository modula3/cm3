
GENERIC INTERFACE Signal(R, P);

TYPE
  IndexType = INTEGER;

  T <: TPublic;
  TPublic = OBJECT
            METHODS
              init      (first, number: IndexType): T;
              fromArray (READONLY arr: P.TBody; first: IndexType := 0): T;
              copy      (): T;

              (*simply 'first' would collide with a instance variable*)
              getFirst  (): IndexType;
              getLast   (): IndexType;
              getNumber (): IndexType;
              getData   (): P.T;

              offset (): R.T;

              upsample   (factor: IndexType): T;
              downsample (factor: IndexType): T;
              wrapCyclic (length: IndexType): T;
              slice      (num: IndexType): REF ARRAY OF T;
              interleave (READONLY slice: ARRAY OF T):
                          T;     (*invocation like init()*)
              reverse (): T;
              adjoint (): T;

              translate (dist: IndexType): T;
              scale     (factor: R.T): T;
              raise     (offset: R.T): T;

              (*inplace operations*)
              translateD (dist: IndexType);
              scaleD     (factor: R.T);
              raiseD     (offset: R.T);

              convolve  (with: T): T;
              superpose (with: T): T;
            END;


END Signal.
