
GENERIC INTERFACE Signal(R, P);

TYPE
  IndexType = INTEGER;

  T <: TPublic;
  TPublic =
    OBJECT
    METHODS
      init      (first, number: IndexType): T;
      initFL    (first, last: IndexType): T;
      fromArray (READONLY arr: P.TBody; first: IndexType := 0): T;
      copy      (): T;

      (*simply 'first' would collide with a instance variable*)
      getFirst  (): IndexType;
      getLast   (): IndexType;
      getNumber (): IndexType;
      getData   (): P.T;

      (*asking for the offset is non-sense since we are working with a
         finite supported signal which has always zero offset*)
      sum (): R.T;

      upsample   (factor: IndexType): T;
      downsample (factor: IndexType): T;
      wrapCyclic (length: IndexType): T;
      slice      (num: IndexType): REF ARRAY OF T;
      interleave (READONLY slice: ARRAY OF T):
                  T;             (*invocation like init()*)
      reverse (): T;
      adjoint (): T;

      translate (dist: IndexType): T;
      scale     (factor: R.T): T;
      (*obviously, raise must be limitted to a finite interval*)
      raise (offset: R.T; first, number: IndexType): T;

      (*inplace operations*)
      translateD (dist: IndexType);
      scaleD     (factor: R.T);

      convolve  (with: T): T;
      superpose (with: T): T;
    END;

PROCEDURE Add (x: T; y: T): T;
PROCEDURE Mul (x: T; y: T): T;


END Signal.
