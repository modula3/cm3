(* $Id$ *)

MODULE RectBinsIndex;
IMPORT MagLayer AS Layer, Word;
IMPORT IntPair;
FROM Fmt IMPORT Int;
IMPORT MagRouteLayer AS RouteLayer;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN IntPair.Equal(a.c , b.c) AND Layer.Equal(a.l, b.l) END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN Word.Times( IntPair.Hash(a.c) , Layer.Hash(a.l) ) END Hash;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN 
    RETURN "(" & Int(a.c.k1) & "," & Int(a.c.k2) & ", \"" & 
           NARROW(a.l,RouteLayer.T).name & "\")" 
  END Format;

BEGIN END RectBinsIndex.
