INTERFACE ProcessSplit;
(* author: thielema *)

(* It is not possible to use a Signal object as input for multiple other
   processes.  This module let you split one stream into multiple ones,
   providing the necessary buffering. *)

IMPORT Signal;

PROCEDURE New (x: Signal.T; number: CARDINAL; ): REF ARRAY OF Signal.T;
(* Create 'number' stream objects holding copies of the input stream.  Only
   generate as much splits as you really need.  An unused copy will cause
   the buffer to overflow. *)

END ProcessSplit.
