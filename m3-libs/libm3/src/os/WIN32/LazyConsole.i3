(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

INTERFACE LazyConsole;

IMPORT File, FileWin32, WinDef;

PROCEDURE New (hd: WinDef.DWORD; ds: FileWin32.DirectionSet): File.T;
(* Returns a file that if ever written to or read from will allocate
   a fresh Windows console for the I/O.  *)

END LazyConsole.
