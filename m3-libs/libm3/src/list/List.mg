(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 22 19:32:33 PDT 1994 by heydon     *)
(*      modified on Wed Apr 28 18:35:20 PDT 1993 by meehan     *)
(*      modified on Tue Apr 27 10:46:04 PDT 1993 by mcjones    *)
(*      modified on Wed Feb 17 21:55:29 PST 1993 by mjordan    *)

GENERIC MODULE List(Elem);

PROCEDURE Cons (READONLY head: Elem.T; tail: T): T =
  BEGIN
    RETURN NEW(T, head := head, tail := tail);
  END Cons;

PROCEDURE List1 (READONLY e1: Elem.T): T =
  BEGIN
    RETURN NEW(T, head := e1, tail := NIL);
  END List1;

PROCEDURE List2 (READONLY e1, e2: Elem.T): T =
  BEGIN
    RETURN NEW(T, head := e1, tail := NEW(T, head := e2, tail := NIL));
  END List2;

PROCEDURE List3 (READONLY e1, e2, e3: Elem.T): T =
  BEGIN
    RETURN NEW(T, head := e1, tail :=
      NEW (T, head := e2, tail := NEW(T, head := e3, tail := NIL)));
  END List3;

PROCEDURE FromArray (READONLY e: ARRAY OF Elem.T): T =
  VAR list: T := NIL; BEGIN
    FOR i := LAST (e) TO FIRST (e) BY -1 DO
      list := NEW (T, head := e [i], tail := list)
    END;
    RETURN list
  END FromArray;

PROCEDURE Length(l: T): CARDINAL=
  VAR i: CARDINAL := 0; BEGIN
    WHILE l # NIL DO
      i := i + 1;
      l := l.tail;
    END;
    RETURN i;
  END Length;

PROCEDURE Nth(l: T; n: CARDINAL): Elem.T=
  BEGIN
    WHILE n > 0 DO
      l := l.tail;
      n := n - 1;
    END;
    RETURN l.head;
  END Nth;

PROCEDURE Member(l: T; READONLY e: Elem.T): BOOLEAN=
  BEGIN
    WHILE l # NIL DO
      IF Elem.Equal(l.head, e) THEN RETURN TRUE; END;
      l := l.tail;
    END;
    RETURN FALSE;
  END Member;

PROCEDURE Append (l1: T; l2: T): T =
  VAR last, rest, result: T;
  BEGIN
    IF l1 = NIL THEN RETURN l2; END;
    IF l2 = NIL THEN RETURN l1; END;

    result := NEW (T, head := l1.head);
    last := result;
    rest := l1.tail;
    WHILE rest # NIL DO
      last.tail := NEW (T, head := rest.head);
      last := last.tail;
      rest := rest.tail;
    END;
    last.tail := l2;

    RETURN result;
  END Append;

PROCEDURE AppendD(l1: T; l2: T): T=
  VAR last: T; BEGIN
    IF l1 = NIL THEN RETURN l2; END;
    IF l2 = NIL THEN RETURN l1; END;
    
    last := l1;
    WHILE last.tail # NIL DO
      last := last.tail;
    END;
    last.tail := l2;

    RETURN l1;
  END AppendD;

PROCEDURE Reverse (l: T): T =
  VAR result: T := NIL; BEGIN
    WHILE l # NIL DO
      result := NEW (T, head := l.head, tail := result);
      l := l.tail;
    END;
    RETURN result;
  END Reverse;

PROCEDURE ReverseD(l: T): T=
  VAR
    next    : T;
    nextTail: T;
  BEGIN
    IF l = NIL THEN RETURN NIL; END;
    next := l.tail;
    l.tail := NIL;
    WHILE next # NIL DO
      nextTail := next.tail;
      next.tail := l;
      l := next;
      next := nextTail;
    END;
    RETURN l;
  END ReverseD;

BEGIN
END List.
