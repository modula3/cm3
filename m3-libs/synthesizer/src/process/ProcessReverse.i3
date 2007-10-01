INTERFACE ProcessReverse;

IMPORT Signal;
IMPORT Thread;

PROCEDURE Do (READONLY x: Signal.Array; ): Signal.RefArray;

TYPE
  T <: Public;
  Public = Signal.T OBJECT
           METHODS
             init (x: Signal.T; ): T
                   RAISES {Signal.End, Signal.Error, Thread.Alerted};
           END;

END ProcessReverse.
