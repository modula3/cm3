(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE LazyConsole;

IMPORT File, FileWin32, WinDef;

PROCEDURE New (hd: WinDef.INT32; ds: FileWin32.DirectionSet): File.T;
(* Returns a file that if ever written to or read from will allocate
   a fresh Windows console for the I/O.  *)

END LazyConsole.
