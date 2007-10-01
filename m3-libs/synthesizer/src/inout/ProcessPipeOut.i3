INTERFACE ProcessPipeOut;
(* author: thielema *)

FROM ProcessInOut IMPORT WordSize;
IMPORT Signal;
IMPORT Wr, Thread;

PROCEDURE Array (READONLY x       : Signal.Array;
                          prog    : TEXT;
                 READONLY params  : ARRAY OF TEXT;
                          wordSize                  := WordSize.Bits16; )
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Stream (         x       : Signal.T;
                           prog    : TEXT;
                  READONLY params  : ARRAY OF TEXT;
                           wordSize                  := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

PROCEDURE MultiStream (READONLY x     : ARRAY OF Signal.T;
                                prog  : TEXT;
                       READONLY params: ARRAY OF TEXT;
                       wordSize := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

END ProcessPipeOut.
