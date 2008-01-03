INTERFACE ProcessLinear;

IMPORT Signal;

TYPE
  T <: Public;
  Public =
    Signal.T OBJECT METHODS init (start, increment: LONGREAL; ): T; END;


END ProcessLinear.
