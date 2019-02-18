(* $Id$ *)

INTERFACE LRElem;
IMPORT Refany;

TYPE
  T = OBJECT x : LONGREAL END;

PROCEDURE Compare(a, b : T) : [-1..1];

CONST Equal = Refany.Equal;

CONST Brand = "LRElem";

END LRElem.
