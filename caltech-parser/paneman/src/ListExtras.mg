(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListExtras.mg,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC MODULE ListExtras(Elem, ElemList);
CONST
  <*NOWARN*>DontWarn = ElemList.Brand;

PROCEDURE MemberDelD(VAR l: T; e: Elem.T): BOOLEAN =
  VAR
    prev,cur: T;
  BEGIN
    IF l = NIL THEN
      RETURN FALSE;
    ELSIF Elem.Equal(l.head, e) THEN
      l := NIL;
      RETURN TRUE;
    ELSE
      prev := l;
      cur := prev.tail;
      WHILE cur # NIL DO
        IF Elem.Equal(cur.head, e) THEN
          prev.tail := cur.tail;
          RETURN TRUE;
        END;
        prev := cur;
        cur := prev.tail;
      END;
      RETURN FALSE;
    END;
  END MemberDelD;

PROCEDURE LastDelD(VAR l: T): Elem.T =
  VAR
    prev, cur: T;
  BEGIN
    <* ASSERT l # NIL *>
    prev := l;
    cur := prev.tail;
    IF cur = NIL THEN
      l := NIL;
      RETURN prev.head;
    ELSE
      WHILE cur.tail # NIL DO
        prev := cur;
        cur := prev.tail;
      END;
      prev.tail := NIL;
      RETURN cur.head;
    END;
  END LastDelD;

PROCEDURE DeleteD(l: T; READONLY e: Elem.T): T =
  VAR
    result := l;
  BEGIN
    EVAL MemberDelD(result, e);
    RETURN result;
  END DeleteD;

BEGIN
END ListExtras.
