(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxFile.i3                                             *)
(* Last modified on Mon Mar 21 15:07:38 PST 1994 by kalsow     *)

INTERFACE MxFile;

IMPORT File, OSError;

PROCEDURE Read (f: File.T; VAR(*OUT*)buf: ARRAY OF CHAR; len: INTEGER): INTEGER
  RAISES {OSError.E};
(* == f.read (LOOPHOLE(buf[0..len-1], ARRAY OF BYTE), mayBlock := TRUE) *)

END MxFile.

