(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE M3Loc;

IMPORT IntRefTbl, Text;
IMPORT M3ID, M3Path;

VAR
  map := NEW (IntRefTbl.Default).init();

PROCEDURE New (pkg, subdir: M3ID.T;  pkg_dir: TEXT): T =
  VAR key := pkg;  ref: REFANY;  t: T;  path: TEXT;
  BEGIN
    IF (key = noPkg) THEN key := subdir; END;

    IF map.get (key, ref) THEN
      (* look for an existing match *)
      t := ref;
      WHILE (t # NIL) DO
        IF (t.subdir = subdir) THEN
          IF (pkg_dir = NIL) # (t.pkg_dir = NIL) THEN
            (* no match *)
          ELSIF (pkg_dir = NIL) THEN
            RETURN t;
          ELSIF Text.Equal (t.pkg_dir, pkg_dir) THEN
            RETURN t;
          END;
        END;
        t := t.next;
      END;

      (* get the full path to this directory *)
      path := M3ID.ToText (subdir);
      IF (pkg_dir # NIL) THEN path := M3Path.New (pkg_dir, path); END;

      (* build a new entry *)
      t := ref;
      t.next := NEW (T, pkg := pkg, subdir := subdir,  pkg_dir := pkg_dir,
                       path := path, next := t.next);
      RETURN t.next;
    END;

    (* get the full path to this directory *)
    path := M3ID.ToText (subdir);
    IF (pkg_dir # NIL) THEN path := M3Path.New (pkg_dir, path); END;
   
    (* this is a new package *)
    t := NEW (T, pkg := pkg, subdir := subdir, pkg_dir := pkg_dir, path := path);
    EVAL map.put (key, t);
    RETURN t;
  END New;

BEGIN
END M3Loc.
