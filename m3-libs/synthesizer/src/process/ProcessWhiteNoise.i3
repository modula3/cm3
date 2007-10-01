INTERFACE ProcessWhiteNoise;

IMPORT Signal;

TYPE
  T <: Public;
  Public =
    Signal.T OBJECT METHODS init (amplitude: LONGREAL; ): T; END;


END ProcessWhiteNoise.
