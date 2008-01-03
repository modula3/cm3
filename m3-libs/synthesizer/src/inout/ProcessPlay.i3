INTERFACE ProcessPlay;
(* author: thielema *)

IMPORT Signal;
IMPORT Wr, Thread;

FROM ProcessInOut IMPORT WordSize;

PROCEDURE Array
  (READONLY x: Signal.Array; rate: LONGREAL; wordSize := WordSize.Bits16; )
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE Stream
  (x: Signal.T; rate: LONGREAL; wordSize := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

PROCEDURE MultiStream (READONLY x   : ARRAY OF Signal.T;
                                rate: LONGREAL;
                       wordSize := WordSize.Bits16; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted};

END ProcessPlay.
