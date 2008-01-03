(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE OS;

IMPORT File, OSError;

PROCEDURE IsDirectory (path: TEXT): BOOLEAN;

PROCEDURE Close (f: File.T;  modTime: LONGREAL;  path: TEXT) RAISES {OSError.E};

END OS.
