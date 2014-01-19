(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jan 27 15:42:58 PST 1995 by kalsow     *)

INTERFACE Utils;

IMPORT File, Wr, Arg, Thread;
FROM Ctypes IMPORT const_char_star, int;

PROCEDURE OpenWriter   (name: TEXT;  fatal: BOOLEAN): Wr.T;
PROCEDURE AppendWriter (name: TEXT;  fatal: BOOLEAN): Wr.T;
PROCEDURE FlushWriter  (wr: Wr.T;  name: TEXT);
PROCEDURE CloseWriter  (wr: Wr.T;  name: TEXT);

PROCEDURE WriteFile    (file: TEXT;  proc: Emitter;  append := FALSE);
TYPE Emitter = PROCEDURE (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};


PROCEDURE OpenReader   (name: TEXT;  fatal: BOOLEAN): File.T;
PROCEDURE CloseReader  (rd: File.T;  name: TEXT);
PROCEDURE RewindReader (rd: File.T;  name: TEXT);

PROCEDURE OpenTempFile (root: TEXT;  VAR(*OUT*) file: TEXT): Wr.T;
PROCEDURE NoteTempFile (name: TEXT);
PROCEDURE RemoveTempFiles ();

PROCEDURE Remove (file: TEXT);
PROCEDURE Copy (old_file, new_file: TEXT);
PROCEDURE IsEqual (a, b: TEXT): BOOLEAN;
PROCEDURE SymbolicLinkFile (from, to: TEXT);
PROCEDURE HardLinkFile (from, to: TEXT);
PROCEDURE IsFile (file: TEXT): BOOLEAN;
PROCEDURE IsDir (file: TEXT): BOOLEAN;

PROCEDURE NoteLocalFileTimes ();
PROCEDURE LocalModTime     (file: TEXT): INTEGER;
PROCEDURE ModificationTime (file: TEXT): INTEGER;
PROCEDURE NoteModification (file: TEXT): INTEGER;
PROCEDURE NoteNewFile      (file: TEXT);

PROCEDURE NoteWidechar16 ();
PROCEDURE NoteWidecharUni ();
PROCEDURE InitWidechar (); 

CONST NO_TIME = 0;

PROCEDURE PrepArgs (program: TEXT;  args: Arg.List): REF ARRAY OF TEXT;
PROCEDURE Execute  (program: TEXT;  args: Arg.List;
                     stdout: TEXT;  fatal: BOOLEAN): INTEGER;

PROCEDURE MakeRelative (VAR from: TEXT;  to: TEXT);
PROCEDURE SymbolicOrHardLink (link: PROCEDURE(name1, name2: const_char_star):int; s_for_sym, from, to: TEXT);

END Utils.
