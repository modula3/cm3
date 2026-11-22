(* $Id: AdGridQ.m3,v 1.1 2002/03/28 21:06:35 mika Exp $ *)

MODULE AdGridQ;
IMPORT LongReal AS LongrealType, Word;
IMPORT AdGridChild AS Child;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN LongrealType.Hash(a.corner(Child.T.UL).x) END Hash;

BEGIN END AdGridQ.
