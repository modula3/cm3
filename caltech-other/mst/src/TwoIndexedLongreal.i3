(* $Id$ *)

INTERFACE TwoIndexedLongreal;

TYPE T = RECORD x, y : INTEGER; d : LONGREAL END;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

CONST Brand = "TwoIndexedLongreal";

END TwoIndexedLongreal.
