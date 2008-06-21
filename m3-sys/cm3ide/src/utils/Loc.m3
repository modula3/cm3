(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Loc;

IMPORT ID, IntRefTbl;

VAR
  map := NEW (IntRefTbl.Default).init ();
  last_t   : T := NIL;
  last_pkg : ID.T := ID.NoID;
  last_dir : ID.T := ID.NoID;

PROCEDURE Add (pkg, subdir: ID.T): T =
  VAR ref: REFANY;  t: T;
  BEGIN
    IF (last_pkg = pkg) AND (last_dir = subdir) THEN
      RETURN last_t;
    END;
    last_pkg := pkg;
    last_dir := subdir;
    IF map.get (pkg, ref) THEN
      (* check for a duplicate *)
      t := ref;
      WHILE (t # NIL) DO
        IF (t.pkg = pkg) AND (t.subdir = subdir) THEN last_t := t;  RETURN t; END;
        t := t.next;
      END;
      (* nope, insert a new one *)
      t := ref;
      t.next := NEW (T, pkg := pkg, subdir := subdir, next := t.next);
      t := t.next;
    ELSE
      t :=  NEW (T, pkg := pkg, subdir := subdir, next := NIL);
      EVAL map.put (pkg, t);
    END;
    last_t := t;
    RETURN t;
  END Add;

BEGIN
END Loc.
