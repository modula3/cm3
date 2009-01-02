(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uutsname;

FROM Ctypes IMPORT char, int;
IMPORT Usysdep;

CONST
  SYS_NMLN = Usysdep.SYS_NMLN;

TYPE
  struct_utsname_star = UNTRACED REF struct_utsname;
  struct_utsname = RECORD
    sysname    : ARRAY [0..SYS_NMLN-1] OF char;
    nodename   : ARRAY [0..SYS_NMLN-1] OF char;
    release    : ARRAY [0..SYS_NMLN-1] OF char;
    version    : ARRAY [0..SYS_NMLN-1] OF char;
    machine    : ARRAY [0..SYS_NMLN-1] OF char;
  END;

<*EXTERNAL*> PROCEDURE uname (buf: struct_utsname_star): int;

END Uutsname.
