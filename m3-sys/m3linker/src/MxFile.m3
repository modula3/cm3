(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxFile.i3                                             *)
(* Last modified on Thu Jul 28 10:22:26 PDT 1994 by kalsow     *)

UNSAFE MODULE MxFile;

IMPORT File, OSError;

CONST BIG = 16_1000000; (* 2^24 => 16M *)
TYPE BufPtr = UNTRACED REF ARRAY [0..BIG-1] OF File.Byte;

PROCEDURE Read (f: File.T; VAR(*OUT*)buf: ARRAY OF CHAR; len: INTEGER): INTEGER
  RAISES {OSError.E} =
  VAR ptr := LOOPHOLE (ADR (buf[0]), BufPtr);
  BEGIN
    RETURN f.read (SUBARRAY (ptr^, 0, MIN (len, NUMBER (buf))),
                   mayBlock := TRUE);
  END Read;

BEGIN
END MxFile.

