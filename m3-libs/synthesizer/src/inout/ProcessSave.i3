INTERFACE ProcessSave;
(* author: thielema *)

FROM ProcessInOut IMPORT WordSize;
IMPORT Signal;
IMPORT Wr, Thread;

PROCEDURE Array (READONLY x       : Signal.Array;
                          rate    : LONGREAL;
                          name    : TEXT;
                          format  : TEXT           := NIL;
                          wordSize                 := WordSize.Bits16; )
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Stream (x       : Signal.T;
                  rate    : LONGREAL;
                  name    : TEXT;
                  format  : TEXT       := NIL;
                  wordSize             := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

PROCEDURE MultiStream (READONLY x     : ARRAY OF Signal.T;
                                rate  : LONGREAL;
                                name  : TEXT;
                                format: TEXT                := NIL;
                       wordSize := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

END ProcessSave.
