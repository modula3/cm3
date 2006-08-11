
GENERIC INTERFACE Signal(R, V);

IMPORT Range, Arithmetic AS Arith;

CONST Brand = R.Brand & "Signal";

TYPE
  IndexType = INTEGER;
  SizeType = CARDINAL;
  ScalingType = [1 .. LAST(CARDINAL)]; (* might be a scaling matrix for
                                          high dimensional signals *)

  SignalPP = ARRAY OF T;         (* VS.TBody *)
  (* discrete parallelepiped containing signals, it is a plain ARRAY in one
     dimension *)

  QuotRem = RECORD quot, rem: T END;


  T <: Public;
  Public =
    OBJECT
    METHODS
      init      (first: IndexType; number: SizeType; ): T;
      initFL    (first, last: IndexType; ): T;
      fromArray (READONLY arr: V.TBody; first: IndexType := 0; ): T;
      fromVector (x: V.T; first: IndexType := 0; ):
                  T;             (* don't modify x afterwards! *)
      copy (): T;

      clip        (first: IndexType; size: SizeType; ): T;
      clipToArray (first: IndexType := 0; VAR arr: V.TBody; );
      (* Take a clip starting at 'first' with length 'NUMBER(arr)' and copy
         it to 'arr'. *)
      clipToVector (first: IndexType; size: SizeType; ): V.T;
      slim         (): T;        (* remove leading and tailing zeros *)

      getFirst (): IndexType;    (* on the one hand 'first' would collide
                                    with an instance variable, on the other
                                    hand 'getFirst' sounds liking peeking
                                    the first value and 'first' would be
                                    conform to the built-in function
                                    FIRST *)
      getLast   (): IndexType;
      getNumber (): IndexType;
      getRange  (): Range.T;

      getData  (): V.T;
      getValue (pos: IndexType; ): R.T;

      equal  (to: T; ): BOOLEAN;
      isZero (): BOOLEAN;

      sum (): R.T;               (* asking for the average offset is
                                    non-sense since we are working with a
                                    finite supported signal which has
                                    always zero mean *)
      inner (with: T; ): R.T;

      upsample   (factor: ScalingType): T;
      downsample (factor: ScalingType): T;
      wrapCyclic (box: ScalingType): V.T;
      slice      (num: ScalingType): REF SignalPP;
      sliceRev   (num: ScalingType): REF SignalPP;
      interleave (READONLY slice: SignalPP): T;
      interleaveRev (READONLY slice: SignalPP):
                     T;          (* invocation like init() *)
      reverse (): T;
      adjoint (): T;

      translate (dist: IndexType): T;
      scale     (factor: R.T): T;
      negate    (): T;
      raise (offset: R.T; first: IndexType; number: SizeType):
             T;                  (* obviously, raise must be limitted to a
                                    finite interval *)

      (* inplace operations, use with care *)
      translateD (dist: IndexType);
      scaleD     (factor: R.T);

      alternate (): T;

      superpose     (with: T): T;
      convolve      (with: T): T;
      autocorrelate (): T;

      convolveDown (with: T; factor: ScalingType):
                    T;           (* convolve and downsampling *)
      upConvolve (with: T; factor: ScalingType):
                  T;             (* convolve with upsampled 'with' *)
      convolveShort (with: T):
                     T;          (* convolve only in the interval where
                                    with 'with' is covered by the signal *)
      extractPeaks (factor: ScalingType):
                    T;           (* keep values at grid points, set the
                                    remaining values to zero; it holds
                                    x.extractPeaks(k) =
                                    x.downSample(k).upsample(k) *)

      deconvolveMod (mask: T; n: CARDINAL; ): QuotRem RAISES {Arith.Error};
      (* deconvolve with remainder, QuotRem{q,r} = x.deconvolve(y,n) means
         x.equal(q.convolve(y).superpose(r)) AND r.getFirst()-x.getFirst()
         = n *)
      deconvolveModAll (mask: T; ): REF ARRAY OF QuotRem
                        RAISES {Arith.Error};
      (* If mask.getNumber() > SELF.getNumber(), then there will be no
         quotient.  Otherwise there are (SELF.getNumber() -
         mask.getNumber() + 1) quotients.  If the quotient for the
         left-most remainder is (a[0],a[1],a[2],...,a[n]) and the quotient
         for the right-most remainder is (b[0],b[1],b[2],...,b[n]), then
         the other quotients are of the form
         (a[0],a[1],...,a[m],b[m+1],...,b[n]).  The function is optimised
         for speed and will accumulate rounding errors in the later
         remainders for non-accurate number types *)
    END;

(* interface compatible to the arithmetic modules *)

VAR Zero, One: T;

PROCEDURE IsZero (x: T): BOOLEAN;
PROCEDURE Equal (x, y: T): BOOLEAN;

PROCEDURE Neg (x: T): T;
PROCEDURE Conj (x: T): T;

PROCEDURE Add (x, y: T): T;
PROCEDURE Sub (x, y: T): T;
PROCEDURE Mul (x, y: T): T;


END Signal.
