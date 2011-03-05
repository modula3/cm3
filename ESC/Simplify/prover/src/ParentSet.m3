(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1996, Digital Equipment Corp. *)

MODULE ParentSet;

IMPORT Enode;

PROCEDURE Empty(): T =
  BEGIN RETURN T{}
  END Empty;

PROCEDURE MakeEmpty(VAR ps: T) =
  BEGIN ps := T{}
  END MakeEmpty;

PROCEDURE IsEmpty(ps: T): BOOLEAN =
  BEGIN RETURN ps = T{}
  END IsEmpty;

PROCEDURE UnionD(VAR ps1: T; ps2: T) =
  BEGIN ps1 := ps1 + ps2
  END UnionD;

PROCEDURE Member(ps: T; id: INTEGER): BOOLEAN =
  BEGIN RETURN (id MOD Width) IN ps
  END Member;

PROCEDURE Overlap(ps1, ps2: T): BOOLEAN =
  BEGIN RETURN (ps1 * ps2) # T{}
  END Overlap;

PROCEDURE AddParentD(VAR res: T; p: Enode.T) =
  BEGIN res := res + T{p.shortId() MOD Width}
  END AddParentD;

PROCEDURE Size(ps: T): CARDINAL =
  VAR res := 0; BEGIN
    FOR i := 0 TO Width-1 DO
      IF i IN ps THEN INC(res) END (* IF *)
    END (* FOR *);
    RETURN res
  END Size;

BEGIN END ParentSet.
