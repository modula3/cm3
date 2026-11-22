(* $Id: AL.m3,v 1.1 2007/06/21 01:16:33 mika Exp $ *)

MODULE AL;
IMPORT Atom;

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

BEGIN END AL.
