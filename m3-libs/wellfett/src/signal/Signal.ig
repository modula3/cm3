
GENERIC INTERFACE Signal(R, P);

TYPE
  IndexType = INTEGER;

  T <: TPublic;
  TPublic = OBJECT
            METHODS
              init      (first, number: IndexType): T;
              initFL    (first, last: IndexType): T;
              fromArray (READONLY arr: P.TBody; first: IndexType := 0): T;
              copy      (): T;

              clipToArray (first: IndexType := 0; VAR arr: P.TBody);
              (*Take a clip starting at 'first' with length 'NUMBER(arr)'
                 and copy it to 'arr'.*)

              getFirst (): IndexType; (*simply 'first' would collide with a
                                         instance variable*)
              getLast   (): IndexType;
              getNumber (): IndexType;
              getData   (): P.T;

              sum (): R.T;       (*asking for the offset is non-sense since
                                    we are working with a finite supported
                                    signal which has always zero offset*)
              inner (with: T): R.T;

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
              negate    (): T;
              raise (offset: R.T; first, number: IndexType):
                     T;          (*obviously, raise must be limitted to a
                                    finite interval*)

              (*inplace operations*)
              translateD (dist: IndexType);
              scaleD     (factor: R.T);

              alternate (): T;

              convolve  (with: T): T;
              superpose (with: T): T;
            END;

VAR One: T;

PROCEDURE Add (x: T; y: T): T;
PROCEDURE Mul (x: T; y: T): T;


END Signal.
