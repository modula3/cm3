(* $Id$ *)

INTERFACE AL;
IMPORT AtomList;

(* utility interface for error messages *)

TYPE T = AtomList.T;

PROCEDURE Format(t : T) : TEXT;

PROCEDURE FromTextArr(READONLY words : ARRAY OF TEXT) : T;

END AL.
