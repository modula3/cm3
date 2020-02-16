(* $Id$ *)
INTERFACE Blockage;
IMPORT GridPoint;

TYPE T = RECORD p : GridPoint.T; args : REFANY END;

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

CONST Brand = "Blockage";

END Blockage.
