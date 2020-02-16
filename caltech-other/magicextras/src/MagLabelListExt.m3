(* $Id$ *)

MODULE MagLabelListExt;
IMPORT MagLabel;
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    WHILE a # NIL DO
      IF b = NIL OR NOT MagLabel.Equal(a.head,b.head) THEN RETURN FALSE END;
      a := a.tail;
      b := b.tail
    END;
    <* ASSERT a = NIL *>
    RETURN b = NIL 
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  VAR 
    res : Word.T := 0;
  BEGIN
    WHILE a # NIL DO 
      res := Word.Plus(res,MagLabel.Hash(a.head)); 
      a := a.tail 
    END;
    RETURN res
  END Hash;

BEGIN END MagLabelListExt.
