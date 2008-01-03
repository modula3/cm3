(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE SeqElem(Elem);


PROCEDURE AddFront(VAR s: T; 
    elem: Elem.T) RAISES {} =
  VAR m := NEW(T);
  BEGIN
    m.elem := elem;
    m.next := s;
    s := m;
  END AddFront;


PROCEDURE Length(t: T): CARDINAL RAISES {}=
  VAR 
    i: CARDINAL := 0;
  BEGIN
    WHILE t # NIL DO
      t := t.next; INC(i);
    END; (* while *)
    RETURN i;
  END Length;

PROCEDURE Empty(s: T): BOOLEAN RAISES {}=
  BEGIN
    RETURN s = NIL;
  END Empty;


PROCEDURE AddRear(VAR s: T; elem: Elem.T)
     RAISES {} =
  VAR
    m := NEW(T);
    t: T;
  BEGIN
    m.elem := elem;
    IF s = NIL THEN
      s := m;
    ELSE
      t := s;
      WHILE t.next # NIL DO
        t := t.next;
      END; (* while *)
      t.next := m;
    END; (* if *)
  END AddRear;

EXCEPTION IsEmpty;

PROCEDURE First(s: T): Elem.T RAISES {} =
  <*FATAL IsEmpty*>
  BEGIN
    IF s = NIL THEN
      RAISE IsEmpty;
    ELSE
      RETURN s.elem;
    END; (* if *)
  END First;


PROCEDURE NewIter(s: T): Iter
     RAISES {} =
  BEGIN
    RETURN s;
  END NewIter;


PROCEDURE Next(VAR iter: Iter; VAR elem: Elem.T)
    : BOOLEAN RAISES {} =
  BEGIN
    IF iter = NIL THEN
      RETURN FALSE;
    ELSE
      elem := iter.elem;
      iter := iter.next;
      RETURN TRUE;
    END; (* if *)
  END Next;

PROCEDURE Exhausted(iter: Iter): BOOLEAN RAISES {}=
  BEGIN
    RETURN iter = NIL;
  END Exhausted;


PROCEDURE Update(<*UNUSED*> VAR s: T; iter: Iter; elem: Elem.T) RAISES {}=
  BEGIN
    iter.elem := elem;
  END Update;

EXCEPTION BoundsError;

PROCEDURE Ith(s: T; i: CARDINAL): Elem.T RAISES {}=
  <*FATAL BoundsError*>
  BEGIN
    FOR j := 1 TO i DO
      IF s # NIL THEN s := s.next END;
    END; (* for *)
    IF s = NIL THEN RAISE BoundsError ELSE RETURN s.elem END;
  END Ith;

PROCEDURE Insert(VAR (*inout*) s: T; elem: Elem.T; i: CARDINAL) RAISES {}=
  VAR t := s; tp: T := NIL;
    <*FATAL BoundsError*>
  BEGIN
    FOR j := 1 TO i DO
      IF t = NIL THEN RAISE BoundsError ELSE tp := t; t := t.next END;
    END; (* for *)
    VAR m := NEW(T);
    BEGIN
      m.elem := elem;
      IF s = NIL THEN s := m 
      ELSE m.next := t; IF tp = NIL THEN s := m ELSE tp.next := m END;
      END;
    END;
  END Insert;



BEGIN

END SeqElem.
