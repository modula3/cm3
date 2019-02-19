(* $Id$ *)

GENERIC INTERFACE SXGenOps(Elem);
IMPORT SXBool, SXInt;

PROCEDURE Choose(on : SXBool.T; ifTrue, ifFalse : Elem.T) : Elem.T;

PROCEDURE Index(on : SXInt.T; READONLY arr : ARRAY OF Elem.T) : Elem.T;
  (* checked runtime error for on to refer outside array *)

END SXGenOps.
