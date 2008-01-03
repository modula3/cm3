(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Sat Sep  4 21:46:59 PDT 1993 by detlefs *)

GENERIC MODULE SetDef(Elem, ElemSet, ElemRefTable);
(* WHERE "ElemSet = Set(Elem)" and "ElemRefTable = Table(Elem, Refany)". *)

REVEAL
  T = Public BRANDED OBJECT
    t: ElemRefTable.Default;
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
    iterate := Iterate;
  END (* OBJECT *);

  Iterator = ElemSet.Iterator BRANDED OBJECT
    tIter: ElemRefTable.Iterator;
   OVERRIDES
    next := Next;
  END (* OBJECT *);

PROCEDURE Init(s: T; sizeHint: CARDINAL): T =
  BEGIN
    s.t := NEW(ElemRefTable.Default).init(sizeHint);
    RETURN s
  END Init;

PROCEDURE FromArray(s: T; READONLY a: ARRAY OF Elem.T): ElemSet.T =
  BEGIN
    s.t := NEW(ElemRefTable.Default).init(NUMBER(a));
    FOR i := 0 TO LAST(a) DO EVAL s.insert(a[i]) END (* FOR *);
    RETURN s
  END FromArray;

PROCEDURE Copy(s: T): ElemSet.T =
  VAR res := NEW(T).init(s.size());
      iter := s.iterate();
      e: Elem.T;
  BEGIN
    WHILE iter.next(e) DO EVAL res.insert(e) END (* WHILE *);
    RETURN res
  END Copy;

PROCEDURE Member(s: T; e: Elem.T): BOOLEAN =
  VAR dummy: REFANY; BEGIN
    RETURN s.t.get(e, dummy)
  END Member;

PROCEDURE Insert(s: T; e: Elem.T): BOOLEAN =
  BEGIN RETURN s.t.put(e, NIL) END Insert;

PROCEDURE Delete(s: T; e: Elem.T): BOOLEAN =
  VAR dummy: REFANY; BEGIN
    RETURN s.t.delete(e, dummy)
  END Delete;

PROCEDURE Size(s: T): CARDINAL =
  BEGIN RETURN s.t.size() END Size;

PROCEDURE Intersection(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR s3: T;
      larger: ElemSet.T;
      smallerIter: ElemSet.Iterator;
      e: Elem.T;
  BEGIN
    IF s1.size() >= s2.size() THEN
      larger := s1; smallerIter := s2.iterate()
    ELSE
      larger := s2; smallerIter := s1.iterate()
    END;
    s3 := NEW(T).init(larger.size());
    WHILE smallerIter.next(e) DO
      IF larger.member(e) THEN EVAL s3.insert(e) END (* IF *)
    END (* WHILE *);
    RETURN s3
  END Intersection;
    
PROCEDURE Diff(s1: T; s2: ElemSet.T): ElemSet.T =
  VAR s3 := NEW(T).init(s1.size());
      iter := s1.iterate();
      e: Elem.T;
  BEGIN
    WHILE iter.next(e) DO
      IF NOT s2.member(e) THEN EVAL s3.insert(e) END (* IF *)
    END (* WHILE *);
    RETURN s3
  END Diff;

CONST SizeFactor = 3;

(* This is overridden because there is a more efficient implementation than
   the default when "s2" is significantly smaller than "s1". *)
PROCEDURE IntersectionD(s1: T; s2: ElemSet.T): ElemSet.T =
  BEGIN
    IF s2.size() * SizeFactor < s1.size() THEN
      VAR tOld := s1.t;
          tNew := NEW(ElemRefTable.Default).init(s2.size());
          iter := s2.iterate();
          e: Elem.T;
          dummy: REFANY;
      BEGIN
        WHILE iter.next(e) DO
          IF tOld.get(e, dummy) THEN EVAL tNew.put(e, NIL) END (* IF *)
        END (* WHILE *);
        s1.t := tNew;
      END (* BEGIN *);
      RETURN s1
    ELSE
      RETURN ElemSet.T.intersectionD(s1, s2)
    END (* IF *)
  END IntersectionD;

PROCEDURE Iterate(s: T): ElemSet.Iterator =
  VAR res := NEW(Iterator, tIter := s.t.iterate()); BEGIN
    RETURN res;
  END Iterate;

PROCEDURE Next(iter: Iterator; VAR e: Elem.T): BOOLEAN =
  VAR dummy: REFANY; BEGIN
    RETURN iter.tIter.next(e, dummy)
  END Next;
    
BEGIN
END SetDef.
