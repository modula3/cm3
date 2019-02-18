(* $Id$ *)

MODULE AL;
IMPORT Atom, AtomList;

PROCEDURE Format(err : T) : TEXT =
  VAR
    msg := "";
    p := err;  
  BEGIN
    WHILE p # NIL DO
      msg := msg & " " & Atom.ToText(p.head); p := p.tail
    END;
    RETURN msg
  END Format;

PROCEDURE FromTextArr(READONLY words : ARRAY OF TEXT) : T =
  VAR res : T := NIL; BEGIN
    FOR i := LAST(words) TO FIRST(words) BY -1 DO
      res := AtomList.Cons(Atom.FromText(words[i]), res)
    END;
    RETURN res
  END FromTextArr;

BEGIN END AL.
