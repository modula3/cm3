INTERFACE ProcessMixer;

IMPORT Signal;

PROCEDURE Do (READONLY x, y: Signal.Array; ): Signal.RefArray;

TYPE
  T <: Public;
  Public = Signal.T OBJECT METHODS init (x, y: Signal.T; ): T; END;

END ProcessMixer.
