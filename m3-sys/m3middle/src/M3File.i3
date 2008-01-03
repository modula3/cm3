(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3File.i3                                             *)
(* Last modified on Wed Feb 22 08:42:27 PST 1995 by kalsow     *)

INTERFACE M3File;

IMPORT File, OSError;

TYPE
  Buffer = ARRAY OF CHAR;
  BufferLength = [0..16_1000000]; (* = 2^24 = 16MBytes *)

PROCEDURE Read (f: File.T; VAR(*OUT*)buf: Buffer; len: BufferLength): INTEGER
  RAISES {OSError.E};
(* == f.read (LOOPHOLE(buf[0..len-1], ARRAY OF File.Byte), mayBlock := TRUE) *)

PROCEDURE Copy (src, dest: TEXT) RAISES {OSError.E};
(* Copy the contents of file "src" to file "dest". *)

PROCEDURE CopyText (src, dest: TEXT;  eol: TEXT) RAISES {OSError.E};
(* Copy the contents of file "src" to file "dest", dropping all
   "\r" characters and mapping "\n" into "eol".  *)

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN RAISES {OSError.E};
(* Return "TRUE" if the files "a" and "b" are equal. *)

PROCEDURE IsDirectory (path: TEXT): BOOLEAN;
(* Return "TRUE" if "path" names a directory. *)

PROCEDURE IsReadable (path: TEXT): BOOLEAN;
(* Return "TRUE" if "path" is a readable file. *)

END M3File.

