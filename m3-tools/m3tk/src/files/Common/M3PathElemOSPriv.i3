(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3PathElemOSPriv;

VAR (*CONST*)
  PathSeparator: CHAR;       (* E.g. ':' for POSIX, ';' for WIN32 *)
  PathnameSeparator: CHAR;   (* E.g. '/' for POSIX, '\' for WIN32 *)
  SCurrentS: TEXT;           (* E.g. "/./" for POSIX, "\.\" for WIN32 *)
  SParentS: TEXT;            (* E.g. "/../" for POSIX, "\..\" for WIN32 *)

END M3PathElemOSPriv.
