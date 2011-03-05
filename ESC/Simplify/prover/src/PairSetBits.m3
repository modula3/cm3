(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1996, Digital Equipment Corp. *)

MODULE PairSetBits;

IMPORT IdSet;

TYPE Rows = [0..IdSet.Width-1];
     IdSetArr = ARRAY Rows OF IdSet.T;

REVEAL T = BRANDED REF IdSetArr;

PROCEDURE Empty(): T =
  VAR res := NEW(T); BEGIN
    res^ := IdSetArr{IdSet.T{}, ..};
    RETURN res
  END Empty;

PROCEDURE SingletonSet(id: INTEGER): IdSet.T =
  VAR hash := id MOD IdSet.Width; BEGIN
    RETURN IdSet.T{hash}
  END SingletonSet;

PROCEDURE MakeEmpty(VAR ps: T) =
  BEGIN
    ps^ := IdSetArr{IdSet.T{}, ..};
  END MakeEmpty;

PROCEDURE Copy(ps: T): T =
  VAR res := Empty(); BEGIN
    FOR i := FIRST(ps^) TO LAST(ps^) DO
      res[i] := ps[i]
    END (* FOR *);
    RETURN res
  END Copy;

PROCEDURE Size(ps: T): INTEGER =
  VAR res := 0; BEGIN
    FOR i := FIRST(ps^) TO LAST(ps^) DO
      FOR j := 0 TO IdSet.Width-1 DO
        IF j IN ps[i] THEN INC(res) END (* IF *)
      END (* FOR *)
    END (* FOR *);
    RETURN res
  END Size;

PROCEDURE IsEmpty(ps: T): BOOLEAN =
  BEGIN
    FOR i := FIRST(ps^) TO LAST(ps^) DO
      IF ps[i] # IdSet.T{} THEN RETURN FALSE END (* IF *)
    END (* FOR *);
    RETURN TRUE
  END IsEmpty;

PROCEDURE AddSetCrossSetD(ps: T; s1, s2: IdSet.T) =
  BEGIN
    FOR i := FIRST(ps^) TO LAST(ps^) DO
      IF i IN s1 THEN ps[i] := ps[i] + s2 END (* IF *)
    END (* FOR *)
  END AddSetCrossSetD;

PROCEDURE AddSetCrossElemD(ps: T; s: IdSet.T; id: INTEGER) =
  VAR s2 := SingletonSet(id); BEGIN
    AddSetCrossSetD(ps, s, s2)
  END AddSetCrossElemD;

PROCEDURE AddElemCrossSetD(ps: T; id: INTEGER; s: IdSet.T) =
  VAR idH := id MOD IdSet.Width; BEGIN
    ps[idH] := ps[idH] + s
  END AddElemCrossSetD;

PROCEDURE UnionD(VAR ps1: T; ps2: T) =
  BEGIN
    FOR i := FIRST(ps1^) TO LAST(ps1^) DO
      ps1[i] := ps1[i] + ps2[i]
    END (* FOR *)
  END UnionD;

PROCEDURE Member(ps: T; id1, id2: INTEGER): BOOLEAN =
  BEGIN
    RETURN (id2 MOD IdSet.Width) IN ps[id1 MOD IdSet.Width]
  END Member;

BEGIN END PairSetBits.
