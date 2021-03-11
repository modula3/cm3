(* $Id$ *)

INTERFACE SchemeInteraction;
IMPORT Scheme;

CONST Brand = "SchemeInteraction";

PROCEDURE Hook(env : REFANY; do : Scheme.Object) : Scheme.Object RAISES { Scheme.E };

END SchemeInteraction.
