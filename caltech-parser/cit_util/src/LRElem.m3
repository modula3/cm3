(* $Id$ *)

MODULE LRElem;
IMPORT LongrealType;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN RETURN LongrealType.Compare(a.x,b.x) END Compare;

BEGIN END LRElem.
