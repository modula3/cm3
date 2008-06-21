(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OS.i3                                                 *)
(* Last modified on Thu Dec  8 08:45:12 PST 1994 by kalsow     *)
(*      modified on Tue Nov 12 14:05:29 PST 1991 by muller     *)


(* This interface defines the OS-specific functions needed by
   the Modula-3 driver. *)

INTERFACE OS;

IMPORT Time, AtomList, Rd, Wr, File;

CONST NO_TIME  = 0;
TYPE  FileTime = INTEGER;

PROCEDURE Now          (): FileTime;
PROCEDURE M3ToFileTime (t: Time.T): FileTime;
PROCEDURE FileToM3Time (t: FileTime): Time.T;
PROCEDURE LastModified (file: TEXT): FileTime;
PROCEDURE FmtFileTime  (t: FileTime): TEXT;

PROCEDURE IsDirectory (file: TEXT): BOOLEAN;
PROCEDURE IsExecutable (file: TEXT): BOOLEAN;
PROCEDURE FileNameEq (a, b: TEXT): BOOLEAN;

PROCEDURE FindExecutable (file: TEXT): TEXT;
(* Return the fully qualified name of "file" on the current
   search path.  Returns "NIL" if no match is found. *)

PROCEDURE CopyDirectory (src, dest: TEXT);
(* Copy the directory "src" and all its contents to "dest". *)

PROCEDURE OpenRd (file: TEXT): Rd.T;
PROCEDURE CloseRd (rd: Rd.T);
PROCEDURE CloseWr (wr: Wr.T);
PROCEDURE CloseFile (f: File.T);
(* Silently close, ignoring any raised exceptions...  *)

PROCEDURE MakePath (a, b, c, d: TEXT := NIL): TEXT;

PROCEDURE Err (ec: AtomList.T): TEXT;

END OS.
