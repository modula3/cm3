(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

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

PROCEDURE MemberDelD(VAR l: T; e: Elem.T): BOOLEAN =
  VAR
    prev,cur: T;
  BEGIN
    IF l = NIL THEN
      RETURN FALSE;
    ELSIF Elem.Equal(l.head, e) THEN
      l := l.tail;
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
  EVAL ElemList.Brand; (* Avoid not used warning *)
END ListF.
