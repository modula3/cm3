(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxSet.m3                                              *)
(* Last Modified On Tue Mar 23 09:20:10 PST 1993 By kalsow     *)

MODULE MxSet;

IMPORT Mx, MxMap;

REVEAL
  T = BRANDED "LinkSet" REF RECORD
    elts: MxMap.T;
  END;
    
TYPE
  Pair = REF RECORD
    name : Mx.Name;
    intf : Elt;
    impl : Elt;
  END;

PROCEDURE New (): T =
  BEGIN
    RETURN NEW (T, elts := MxMap.New (17));
  END New;

PROCEDURE Insert (t: T;  e: Elt) =
  VAR p: Pair := MxMap.Get (t.elts, e.name);
  BEGIN
    IF (p = NIL) THEN
      p := NEW (Pair, name := e.name);
      MxMap.Insert (t.elts, p.name, p);
    END;
    IF (e.interface)
      THEN p.intf := e;
      ELSE p.impl := e;
    END;
  END Insert;

PROCEDURE IsMember (t: T;  e: Elt): BOOLEAN =
  VAR p: Pair := MxMap.Get (t.elts, e.name);
  BEGIN
    IF    (p = NIL)     THEN RETURN FALSE
    ELSIF (e.interface) THEN RETURN (p.intf # NIL);
    ELSE                     RETURN (p.impl # NIL);
    END;
  END IsMember;

PROCEDURE ToList (t: T): Mx.UnitList =
  VAR x: MxMap.Contents;  u: Mx.UnitList := NIL;  p: Pair;
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    x := MxMap.GetData (t.elts);
    IF (x = NIL) THEN RETURN NIL END;
    FOR i := 0 TO LAST (x^) DO
      p := x[i].value;
      IF (p # NIL) THEN
        IF (p.intf # NIL) THEN
          u := NEW (Mx.UnitList, unit := p.intf, next := u);
        END;
        IF (p.impl # NIL) THEN
          u := NEW (Mx.UnitList, unit := p.impl, next := u);
        END;
      END;
    END;
    RETURN u;
  END ToList;

BEGIN
END MxSet.

