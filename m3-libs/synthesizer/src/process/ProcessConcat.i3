INTERFACE ProcessConcat;
(* author: thielema *)

(* Concatenate a sequence of several sounds. *)

IMPORT Signal;

TYPE
  T <: Public;
  Public =
    Signal.T OBJECT METHODS init (READONLY x: ARRAY OF Signal.T; ): T; END;

END ProcessConcat.
