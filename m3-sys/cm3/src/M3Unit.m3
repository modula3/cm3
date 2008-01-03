(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Unit;

IMPORT IntRefTbl, M3ID, M3Loc, M3Path;
IMPORT Msg;

PROCEDURE InitSet (VAR x: Set) =
  BEGIN
    x.map  := NEW (IntRefTbl.Default).init ();
    x.head := NIL;
    x.tail := NIL;
  END InitSet;

PROCEDURE Add (VAR x: Set;  t: T) =
  VAR ref: REFANY;  tt: T;
  BEGIN
    (* update the name table *)
    IF x.map.get (t.name, ref) THEN
      (* check for an existing match *)
      tt := NARROW (ref, T);
      WHILE (tt # NIL) DO
        IF (tt.name = t.name) AND (tt.kind = t.kind) THEN
          IF (tt.loc # t.loc) THEN
            Msg.FatalError (NIL, "duplicate unit: ", FullPath (tt),
                                                " ", FullPath (t));
          END;
          RETURN;
        END;
        tt := tt.next_synonym;
      END;

      (* insert into the existing list *)
      <*ASSERT t.next_synonym = NIL*>
      tt := NARROW (ref, T);
      t.next_synonym := tt.next_synonym;
      tt.next_synonym := t;
    ELSE
      (* start a new list *)
      EVAL x.map.put (t.name, t);
    END;

    (* append to the linked list *)
    IF (x.head = NIL)
      THEN x.head := t;
      ELSE x.tail.next := t;
    END;
    x.tail := t;
  END Add;

PROCEDURE Get (READONLY x: Set;  nm: M3ID.T;  k: Kind): T =
  VAR ref: REFANY;  t: T;
  BEGIN
    IF x.map.get (nm, ref) THEN
      t := ref;
      WHILE (t # NIL) DO
        IF (t.kind = k) THEN RETURN t; END;
        t := t.next_synonym;
      END;
    END;
    RETURN NIL;
  END Get;

PROCEDURE New (nm: M3ID.T;  k: Kind;  loc: M3Loc.T;  hidden, imported: BOOLEAN): T =
  BEGIN
    RETURN NEW (T, name := nm, kind := k, loc := loc,
                hidden := hidden, imported := imported);
  END New;

PROCEDURE AddNew (VAR x: Set;  nm: M3ID.T;  k: Kind;  loc: M3Loc.T;
                  hidden, imported: BOOLEAN) =
  BEGIN
    Add (x, New (nm, k, loc, hidden, imported));
  END AddNew;

PROCEDURE FileName (t: T): TEXT =
  VAR nm := M3ID.ToText (t.name);
  BEGIN
    RETURN M3Path.Join (NIL, nm, t.kind, host := TRUE);
  END FileName;

PROCEDURE FullPath (t: T): TEXT =
  VAR nm := M3ID.ToText (t.name);
  BEGIN
    RETURN M3Path.Join (t.loc.path, nm, t.kind, host := TRUE);
  END FullPath;

BEGIN
END M3Unit.
