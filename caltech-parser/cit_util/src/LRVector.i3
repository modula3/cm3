(* $Id: LRVector.i3,v 1.3 2005/04/25 21:47:17 mika Exp $ *)

INTERFACE LRVector;
IMPORT Word;

TYPE S = ARRAY OF LONGREAL;
TYPE T = REF S;

CONST Brand = "LRVector";

PROCEDURE Norm(v : T) : LONGREAL;

PROCEDURE Copy(a : T) : T;

PROCEDURE Equal(a, b : T) : BOOLEAN;
  (* value equality, not reference equality.  a, b must not be NIL *)

PROCEDURE Hash(a : T) : Word.T;
  (* slow hash.  a must not be NIL *)

PROCEDURE Compare(a, b : T) : [-1..1];
  (* compare two vectors by entries in order -- both must be non-NIL and 
     same # of elements *)

END LRVector.
