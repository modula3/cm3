
GENERIC INTERFACE Signal(R, V);

CONST Brand = "Signal";

TYPE
  IndexType = INTEGER;
  SizeType = CARDINAL;
  ScalingType = CARDINAL;        (*might be a scaling matrix for high
                                    dimensional signals*)
  SignalPP = ARRAY OF T;         (*VS.TBody*)(*discrete parallelepiped
                                                containing signals, it is a
                                                plain ARRAY in one
                                                dimension*)

  T <: TPublic;
  TPublic =
    OBJECT
    METHODS
      init      (first: IndexType; number: SizeType): T;
      initFL    (first, last: IndexType): T;
      fromArray (READONLY arr: V.TBody; first: IndexType := 0): T;
      fromVector (x: V.T; first: IndexType := 0):
                  T;             (*don't modify x afterwards!*)
      copy (): T;

      clip        (first: IndexType; size: SizeType): T;
      clipToArray (first: IndexType := 0; VAR arr: V.TBody);
      (*Take a clip starting at 'first' with length 'NUMBER(arr)' and copy
         it to 'arr'.*)
      clipToVector (first: IndexType; size: SizeType): V.T;

      getFirst (): IndexType;    (*simply 'first' would collide with a
                                    instance variable*)
      getLast   (): IndexType;
      getNumber (): IndexType;
      getData   (): V.T;
      getValue  (pos: IndexType): R.T;

      equal  (to: T): BOOLEAN;
      isZero (): BOOLEAN;

      sum (): R.T;               (*asking for the average offset is
                                    non-sense since we are working with a
                                    finite supported signal which has
                                    always zero mean*)
      inner (with: T): R.T;

      upsample   (factor: ScalingType): T;
      downsample (factor: ScalingType): T;
      wrapCyclic (box: ScalingType): T;
      slice      (num: ScalingType): REF SignalPP;
      sliceRev   (num: ScalingType): REF SignalPP;
      interleave (READONLY slice: SignalPP): T;
      interleaveRev (READONLY slice: SignalPP):
                     T;          (*invocation like init()*)
      reverse (): T;
      adjoint (): T;

      translate (dist: IndexType): T;
      scale     (factor: R.T): T;
      negate    (): T;
      raise (offset: R.T; first: IndexType; number: SizeType):
             T;                  (*obviously, raise must be limitted to a
                                    finite interval*)

      (*inplace operations, use with care*)
      translateD (dist: IndexType);
      scaleD     (factor: R.T);

      alternate (): T;

      superpose     (with: T): T;
      convolve      (with: T): T;
      autocorrelate (): T;

      convolveDown (with: T; factor: ScalingType):
                    T;           (*convolve and downsampling*)
      upConvolve (with: T; factor: ScalingType):
                  T;             (*convolve with upsampled 'with'*)
      convolveShort (with: T): T; (*convolve only in the interval where
                                     with 'with' is covered by the signal*)
    END;

(*interface compatible to the arithmetic modules*)

VAR Zero, One: T;

PROCEDURE IsZero (x: T): BOOLEAN;
PROCEDURE Equal (x, y: T): BOOLEAN;

PROCEDURE Neg (x: T): T;
PROCEDURE Conj (x: T): T;

PROCEDURE Add (x, y: T): T;
PROCEDURE Sub (x, y: T): T;
PROCEDURE Mul (x, y: T): T;


END Signal.
