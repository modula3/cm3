(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Jul 25 11:51:56 PDT 1994 by detlefs *)

GENERIC MODULE SetList(Elem, ElemSet, ElemList);
(* WHERE "ElemSet = Set(Elem)", and "ElemList = List(Elem)". *)

REVEAL
  T = Public BRANDED OBJECT
    l: ElemList.T := NIL
   OVERRIDES
    init := Init;
    fromArray := FromArray;
    copy := Copy;
    member := Member;
    insert := Insert;
    delete := Delete;
    size := Size;
    intersection := Intersection;
    diff := Diff;
    intersectionD := IntersectionD;
    diffD := DiffD;
    iterate := Iterate;
  END (* OBJECT *);

  Iterator = ElemSet.Iterator BRANDED OBJECT
    l: ElemList.T
   OVERRIDES
    next := Next;
  END (* OBJECT *);

PROCEDURE Init(s: T): T =
  BEGIN RETURN s END Init;

PROCEDURE FromArray(s: T; READONLY a: ARRAY OF Elem.T): ElemSet.T =
  BEGIN
    s.l := NIL;
    FOR i := 0 TO LAST(a) DO EVAL s.insert(a[i]) END (* FOR *);
    RETURN s
  END FromArray;

PROCEDURE Copy(s: T): ElemSet.T =
  VAR res := NEW(T).init(); rl := s.l; BEGIN
    WHILE rl # NIL DO
      res.l := ElemList.Cons(rl.head, res.l);
      rl := rl.tail
    END (* WHILE *);
    RETURN res
  END Copy;

PROCEDURE Member(s: T; e: Elem.T): BOOLEAN =
  VAR rl := s.l; BEGIN
    WHILE rl # NIL DO
      IF Elem.Equal(rl.head, e) THEN RETURN TRUE END (* IF *);
      rl := rl.tail
    END (* WHILE *);
    RETURN FALSE
  END Member;

PROCEDURE Insert(s: T; e: Elem.T): BOOLEAN =
  BEGIN
    IF s.member(e) THEN
      RETURN TRUE
    ELSE
      s.l := ElemList.Cons(e, s.l);
      RETURN FALSE
    END (* IF *)
  END Insert;

PROCEDURE Delete(s: T; e: Elem.T): BOOLEAN =
  VAR pl: ElemList.T := NIL; rl := s.l; BEGIN
    WHILE rl # NIL DO
      IF Elem.Equal(e, rl.head) THEN
        IF pl = NIL THEN
          s.l := rl.tail
        ELSE
          pl.tail := rl.tail
        END (* IF *);
        RETURN TRUE
      ELSE
        pl := rl;
        rl := rl.tail
      END (* IF *)
    END (* WHILE *);
    RETURN FALSE
  END Delete;

PROCEDURE Size(s: T): CARDINAL =
  BEGIN RETURN ElemList.Length(s.l) END Size;

PROCEDURE Intersection(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR rl1 := s1.l; res := NEW(T).init(); BEGIN
    WHILE rl1 # NIL DO
      IF s2.member(rl1.head) THEN
        res.l := ElemList.Cons(rl1.head, res.l)
      END (* IF *);
      rl1 := rl1.tail
    END (* WHILE *);
    RETURN res
  END Intersection;
    
PROCEDURE Diff(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR rl1 := s1.l; res := NEW(T).init(); BEGIN
    WHILE rl1 # NIL DO
      IF NOT s2.member(rl1.head) THEN
        res.l := ElemList.Cons(rl1.head, res.l)
      END (* IF *);
      rl1 := rl1.tail
    END (* WHILE *);
    RETURN res
  END Diff;
    
PROCEDURE IntersectionD(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR prl1: ElemList.T := NIL;
      rl1 := s1.l;
  BEGIN
    WHILE rl1 # NIL DO
      IF NOT s2.member(rl1.head) THEN
        IF prl1 = NIL THEN
          s1.l := rl1.tail
        ELSE
          prl1.tail := rl1.tail
        END (* IF *)
      ELSE
        prl1 := rl1
      END (* IF *);
      rl1 := rl1.tail
    END (* WHILE *);
    RETURN s1
  END IntersectionD;

PROCEDURE DiffD(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR prl1: ElemList.T := NIL;
      rl1 := s1.l;
  BEGIN
    WHILE rl1 # NIL DO
      IF s2.member(rl1.head) THEN
        IF prl1 = NIL THEN
          s1.l := rl1.tail
        ELSE
          prl1.tail := rl1.tail
        END (* IF *)
      ELSE
        prl1 := rl1
      END (* IF *);
      rl1 := rl1.tail
    END (* WHILE *);
    RETURN s1
  END DiffD;

PROCEDURE Iterate(s: T): ElemSet.Iterator =
  VAR res := NEW(Iterator, l := s.l); BEGIN
    RETURN res;
  END Iterate;

PROCEDURE Next(iter: Iterator; VAR e: Elem.T): BOOLEAN =
  BEGIN
    IF iter.l = NIL THEN
      RETURN FALSE
    ELSE
      e := iter.l.head;
      iter.l := iter.l.tail;
      RETURN TRUE
    END (* IF *)
  END Next;
    
BEGIN
END SetList.

  
    
