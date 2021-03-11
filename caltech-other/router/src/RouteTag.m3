(* $Id$ *)

MODULE RouteTag;
IMPORT Word;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;
PROCEDURE Equal(a, b : T) : BOOLEAN = 
  VAR
    res := a.equal(b);
  BEGIN 
    IF res THEN <* ASSERT Hash(a) = Hash(b) *> END;
    RETURN res
  END Equal;

BEGIN END RouteTag.
