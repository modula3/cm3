INTERFACE ProcessPlot;
(* author: thielema *)

IMPORT Signal;
IMPORT Thread;


PROCEDURE Array (READONLY x     : Signal.Array;
                          rate  : LONGREAL       := 1.0D0;
                          window: CARDINAL       := 500;
                          yRange: LONGREAL       := 1.0D0; );

PROCEDURE Stream (x     : Signal.T;
                  rate  : LONGREAL   := 1.0D0;
                  window: CARDINAL   := 500;
                  yRange: LONGREAL   := 1.0D0; )
  RAISES {Signal.Error, Thread.Alerted};

PROCEDURE MultiStream (READONLY x     : ARRAY OF Signal.T;
                                rate  : LONGREAL            := 1.0D0;
                                window: CARDINAL            := 500;
                                yRange: LONGREAL            := 1.0D0; )
  RAISES {Signal.Error, Thread.Alerted};

END ProcessPlot.
