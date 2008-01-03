(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sun May 14 19:57:51 PDT 1995 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "Set" is a generic interface defining sets of "Elem.T"'s. *)

GENERIC MODULE Set(Elem);

(* Set is an abstract type, but many of it's operations can be implemented
   reasonably efficiently independently of any specific representation
   decisison.  Those are defined here.  Of course, subtypes may choose
   representations that offer the possibility of more efficient
   implementations, and would therefore override these methods.
*)

REVEAL
  T = Public BRANDED OBJECT
   OVERRIDES
    isEmpty := IsEmpty;
    subset := Subset;
    equal := Equal;
    intersect := Intersect;
    union := Union;
    intersection := Intersection;
    diff := Diff;
    unionD := UnionD;
    intersectionD := IntersectionD;
    diffD := DiffD;
  END;

PROCEDURE IsEmpty(s: T): BOOLEAN =
  BEGIN RETURN s.size() = 0 END IsEmpty;

PROCEDURE Subset(s: T; s2: T): BOOLEAN =
  BEGIN
    IF s.size() > s2.size() THEN
      RETURN FALSE
    ELSE
      VAR iter := s.iterate();
          e: Elem.T;
      BEGIN
        WHILE iter.next(e) DO
          IF NOT s2.member(e) THEN RETURN FALSE END (* IF *)
        END (* WHILE *)
      END (* BEGIN *);
      RETURN TRUE
    END (* IF *)
  END Subset;

PROCEDURE Equal(s1, s2: T): BOOLEAN =
  BEGIN
    RETURN s1.size() = s2.size() AND s1.subset(s2) AND s2.subset(s1)
  END Equal;

PROCEDURE Intersect(s: T; s2: T): BOOLEAN =
  BEGIN
    (* Make "s" the smaller. *)
    IF s.size() > s2.size() THEN
      VAR tmp := s; BEGIN s := s2; s := tmp END
    END (* IF *);
    VAR iter := s.iterate();
        e: Elem.T;
    BEGIN
      WHILE iter.next(e) DO
        IF s2.member(e) THEN RETURN TRUE END (* IF *)
      END (* WHILE *);
      RETURN FALSE
    END (* BEGIN *)
  END Intersect;

PROCEDURE Union(s1: T; s2: T): T =
  VAR s3 := s1.copy(); BEGIN
    RETURN s3.unionD(s2)
  END Union;
    
PROCEDURE Intersection(s1: T; s2: T): T =
  VAR s3 := s1.copy(); BEGIN
    RETURN s3.intersectionD(s2)
  END Intersection;
    
PROCEDURE Diff(s1: T; s2: T): T =
  VAR s3 := s1.copy(); BEGIN
    RETURN s3.diffD(s2)
  END Diff;
    
PROCEDURE UnionD(s1: T; s2: T): T =
  VAR iter := s2.iterate(); e: Elem.T; BEGIN
    WHILE iter.next(e) DO EVAL s1.insert(e) END (* WHILE *);
    RETURN s1
  END UnionD;

PROCEDURE IntersectionD(s1: T; s2: T): T =
  VAR iter := s1.iterate(); e: Elem.T; BEGIN
    WHILE iter.next(e) DO
      IF NOT s2.member(e) THEN EVAL s1.delete(e) END (* IF *)
    END (* WHILE *);
    RETURN s1
  END IntersectionD;

PROCEDURE DiffD(s1: T; s2: T): T =
  VAR iter := s1.iterate(); e: Elem.T; BEGIN
    WHILE iter.next(e) DO
      IF s2.member(e) THEN EVAL s1.delete(e) END (* IF *)
    END (* WHILE *);
    RETURN s1
  END DiffD;

BEGIN
END Set.


