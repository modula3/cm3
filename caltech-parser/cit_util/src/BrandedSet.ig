(* $Id: BrandedSet.ig,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)
GENERIC INTERFACE BrandedSet(Elem, Set);
IMPORT Word;

TYPE T = Set.T;

CONST Brand = "Set of " & Elem.Brand;

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T; (* XXX inefficient *)

END BrandedSet.
