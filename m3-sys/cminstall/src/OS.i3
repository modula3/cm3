(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE OS;

IMPORT AtomList, CoffTime;

CONST
  on_unix = (CoffTime.EpochAdjust = 0.0d0);

PROCEDURE IsDirectory (file: TEXT): BOOLEAN;
PROCEDURE IsExecutable (file: TEXT): BOOLEAN;
PROCEDURE FileNameEq (a, b: TEXT): BOOLEAN;
PROCEDURE CleanDirName (dir: TEXT): TEXT;

PROCEDURE FindExecutable (file: TEXT): TEXT;
(* Return the fully qualified name of "file" on the current
   search path.  Returns "NIL" if no match is found. *)

PROCEDURE MakePath (a, b, c, d: TEXT := NIL): TEXT;
PROCEDURE MakeDir (dir: TEXT): BOOLEAN;

PROCEDURE GetAbsolutePath (a, b: TEXT := NIL): TEXT;

PROCEDURE GetDiskSpace (dir: TEXT): INTEGER;

PROCEDURE WriteFile (name, contents: TEXT);
PROCEDURE RemoveFile (file: TEXT);
PROCEDURE MoveFile (src, dest: TEXT);

PROCEDURE Err (ec: AtomList.T): TEXT;

END OS.
