(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE OS;

IMPORT File, OSError;

PROCEDURE IsDirectory (path: TEXT): BOOLEAN;

PROCEDURE Close (f: File.T;  modTime: LONGREAL;  path: TEXT) RAISES {OSError.E};

END OS.
