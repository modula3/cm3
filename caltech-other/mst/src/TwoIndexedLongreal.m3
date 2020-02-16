(* $Id$ *)

MODULE TwoIndexedLongreal;
IMPORT LongrealType;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] = 
  BEGIN RETURN LongrealType.Compare(a.d, b.d) END Compare;

BEGIN END TwoIndexedLongreal.
