(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug 21 15:00:56 PDT 1992 by heydon                   *)

MODULE AtomJVTbl;

IMPORT Atom, TxtRefTbl, JunoValue AS Value;

REVEAL
  T = Public BRANDED OBJECT
    tbl: TxtRefTbl.T;
  OVERRIDES
    init := Init;
    get := Get;
    put := Put;
    delete := Delete;
    map := Map;
  END;

PROCEDURE Init(t: T; size: INTEGER): T =
  BEGIN
    t.tbl := TxtRefTbl.New(size);
    RETURN t
  END Init;

PROCEDURE Get(t: T; a: Atom.T; VAR (* OUT *) p: Value.T): BOOLEAN =
  BEGIN
    RETURN t.tbl.in(Atom.Name(a), p)
  END Get;

PROCEDURE Put(t: T; a: Atom.T; p: Value.T): BOOLEAN =
  BEGIN
    RETURN t.tbl.put(Atom.Name(a), p)
  END Put;

PROCEDURE Delete(t: T; a: Atom.T; VAR (* OUT *) p: Value.T): BOOLEAN =
  BEGIN
    RETURN t.tbl.delete(Atom.Name(a), p)
  END Delete;

PROCEDURE Map(t: T; p: MapProc) =
VAR dummy1: TEXT; dummy2: Value.T;

    PROCEDURE Enumerate(<*UNUSED*>dummy: REFANY; key: TEXT;
			VAR value: Value.T) : BOOLEAN =
    BEGIN
	p(Atom.New(key), value);
	RETURN FALSE
    END Enumerate;

BEGIN
    EVAL t.tbl.enumerate(Enumerate, NIL, dummy1, dummy2);
END Map;

BEGIN
END AtomJVTbl.
