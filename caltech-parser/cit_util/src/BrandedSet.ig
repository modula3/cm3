(* $Id: BrandedSet.ig,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)
GENERIC INTERFACE BrandedSet(Elem, Set);
IMPORT Word;

TYPE T = Set.T;

CONST Brand = "Set of " & Elem.Brand;

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T; (* XXX inefficient *)

END BrandedSet.
