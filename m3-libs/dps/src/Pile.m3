(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:27:37 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:22 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


MODULE Pile;

IMPORT Text;

PROCEDURE New (): T =
  BEGIN
  RETURN NEW (T, first := NIL, mutex := NEW (MUTEX));
  END New; 

PROCEDURE Insert (pile: T; it: Element) =
  BEGIN
  LOCK pile.mutex DO
    it.next := pile.first;
    pile.first := it;
    END;
  END Insert;
 
PROCEDURE Delete (pile: T; it: Element) =
 VAR one: Element;
  BEGIN
  LOCK pile.mutex DO
    one := pile.first;
    <* ASSERT one # NIL *> (* Client error: Delete from empty pile! *)
    IF one = it THEN pile.first := one.next; RETURN; END;
    WHILE one.next#NIL DO
      IF one.next = it THEN one.next := one.next.next; RETURN; END;
      one := one.next;
      END;
    <* ASSERT FALSE *> (* Client error: Delete-ee not in pile! *)
    END; (* of LOCK *)
  END Delete; 

PROCEDURE FindByKey (pile: T; key: INTEGER): Element =
 VAR one: Element;
  BEGIN
  LOCK pile.mutex DO
    one := pile.first;
    WHILE one#NIL DO
      IF one.key = key THEN RETURN one; END;
      one := one.next;
      END;
    END;
  RETURN NIL;
  END FindByKey; 

PROCEDURE FindByName (pile: T; name: TEXT): Element =
 VAR one: Element;
  BEGIN
  LOCK pile.mutex DO
    one := pile.first;
    WHILE one#NIL DO
      IF Text.Equal(one.name, name) THEN RETURN one; END;
      one := one.next;
      END;
    END;
  RETURN NIL;
  END FindByName; 

  BEGIN

  END Pile.

