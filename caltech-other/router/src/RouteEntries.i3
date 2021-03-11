(* $Id$ *)

INTERFACE RouteEntries;
FROM EndPointStatus IMPORT Dir;

TYPE T = SET OF Dir;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "RouteEntries";

PROCEDURE Format(READONLY a : T) : TEXT;

END RouteEntries.
