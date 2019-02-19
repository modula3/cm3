(* $Id$ *)

MODULE AdGridQ;
IMPORT LongrealType, Word;
IMPORT AdGridChild AS Child;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN LongrealType.Hash(a.corner(Child.T.UL).x) END Hash;

BEGIN END AdGridQ.
