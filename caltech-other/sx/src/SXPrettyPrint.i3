(* $Id$ *)

INTERFACE SXPrettyPrint;
IMPORT Wr, SX, Thread;

PROCEDURE Put(wr : Wr.T; sx : SX.T) RAISES { Wr.Failure, Thread.Alerted };

END SXPrettyPrint.
