(* $Id$ *)
INTERFACE MagPointDist;
IMPORT MagPoint;

TYPE
  T = RECORD p : MagPoint.T; dist : CARDINAL END;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

CONST Brand = "MagPointDist";

END MagPointDist.
