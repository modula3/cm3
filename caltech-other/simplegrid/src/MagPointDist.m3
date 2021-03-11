(* $Id$ *)
MODULE MagPointDist;
IMPORT Integer;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.dist,b.dist) END Compare;

BEGIN END MagPointDist.
