(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE OS;

IMPORT AtomList, CoffTime;

EXCEPTION Error(TEXT);

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

PROCEDURE FilenameWithoutSpaces (fn: TEXT): TEXT;

PROCEDURE GetShortFilename (longFilename: TEXT): TEXT RAISES {Error};
  (* Return a mangled filename without spaces (WIN32). *)

END OS.
