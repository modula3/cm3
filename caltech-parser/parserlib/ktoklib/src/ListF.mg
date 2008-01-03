(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListF.mg,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

GENERIC MODULE ListF(Elem, ElemList);
PROCEDURE Format(l: T): TEXT =
  VAR
    cur: T;
    result: TEXT;
  BEGIN
    IF l = NIL THEN
      RETURN "()";
    ELSE
      result := "(" & Elem.Format(l.head);
      cur := l.tail;
      WHILE cur # NIL DO
        result := result & "," & Elem.Format(cur.head);
        cur := cur.tail;
      END;
      RETURN result & ")";
    END;
  END Format;

PROCEDURE Equal(l1,l2: T): BOOLEAN =
  VAR
    cs := l1;
    ct := l2;
  BEGIN
    IF (cs # NIL) # (ct # NIL) THEN
      RETURN FALSE;
    END;
    WHILE cs # NIL DO
      IF ct = NIL THEN
        RETURN FALSE;
      END;
      IF NOT Elem.Equal(ct.head, cs.head) THEN
         RETURN FALSE;
      END;
      ct := ct.tail;
      cs := cs.tail;
    END;
    RETURN ct = NIL;
  END Equal;

PROCEDURE Hash(l: T): INTEGER = 
  VAR
    cur := l;
    acc: INTEGER := 0;
  BEGIN
    WHILE cur # NIL DO
      acc := acc*3 + Elem.Hash(cur.head);
      cur := cur.tail;
    END;
    RETURN acc;
  END Hash;

BEGIN
  EVAL ElemList.Brand; (* Avoid not used warning *)
END ListF.
