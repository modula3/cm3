(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NMonRegistrySvr.m3 *)
(* Last modified on Wed Sep  8 13:37:17 PDT 1993 by wobber *)
(*      modified on Tue Sep 15 11:07:45 PDT 1992 by evers  *)

(* An implementation of the "NetObjMon.Registry" service. *)

MODULE NMonRegistrySvr;

IMPORT NetObjMon, NetObj, NetObjNotifier;

IMPORT TextRefTbl;

<* PRAGMA LL *>

TYPE
  T = NetObjMon.Registry OBJECT
    mu: MUTEX;
    <* LL >= {mu} *>
    entries: TextRefTbl.T (* of NetObjMon.T *) := NIL;
  OVERRIDES
    register := RegistryRegister;
    list     := RegistryList;
    get      := RegistryGet;
  END;

  Note = NetObjNotifier.NotifierClosure OBJECT
    t: T;
    id: TEXT;
  OVERRIDES
    notify := DeadEntry;
  END;

PROCEDURE New (): NetObjMon.Registry =
  BEGIN
    RETURN NEW (T, mu := NEW (MUTEX),
                   entries := NEW(TextRefTbl.Default).init());
  END New;

PROCEDURE DeadEntry(n: Note; obj: NetObj.T;
                    <*UNUSED*> st: NetObjNotifier.OwnerState) =
  VAR r: REFANY;
  BEGIN
    LOCK n.t.mu DO
      IF n.t.entries.get(n.id, r) AND r = obj THEN
        EVAL n.t.entries.delete(n.id, r);
      END;
    END;
  END DeadEntry;

PROCEDURE RegistryRegister (t: T; mon: NetObjMon.T; id: TEXT) =
  BEGIN
    IF mon # NIL THEN
      NetObjNotifier.AddNotifier(mon, NEW(Note, t := t, id := id));
    END;
    LOCK t.mu DO EVAL t.entries.put (id, mon); END;
  END RegistryRegister;

PROCEDURE RegistryList (t: T): REF ARRAY OF TEXT =
  VAR
    res: REF ARRAY OF TEXT;
  BEGIN
    LOCK t.mu DO
      res := NEW (REF ARRAY OF TEXT, t.entries.size());
      VAR it := t.entries.iterate();
          i := 0;
          r: REFANY;
          txt: TEXT;
      BEGIN
        WHILE it.next(txt, r) DO res[i] := txt; INC(i); END;
      END;
    END;
    RETURN res;
  END RegistryList;

PROCEDURE RegistryGet (t: T; id: TEXT): NetObjMon.T =
  VAR
    res: REFANY;
  BEGIN
    LOCK t.mu DO
      IF t.entries.get (id, res) THEN
      	RETURN res;
      ELSE
      	RETURN NIL;
      END;
    END;
  END RegistryGet;

BEGIN
END NMonRegistrySvr.
