(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

INTERFACE Uutsname;

FROM Ctypes IMPORT char, int;

CONST
  SYS_NMLN = 32;

TYPE
  struct_utsname_star = UNTRACED REF struct_utsname;
  struct_utsname = RECORD
    sysname   : ARRAY [0..SYS_NMLN-1] OF char;
    nodename  : ARRAY [0..SYS_NMLN-1] OF char;
    release   : ARRAY [0..SYS_NMLN-1] OF char;
    version   : ARRAY [0..SYS_NMLN-1] OF char;
    machine   : ARRAY [0..SYS_NMLN-1] OF char;
  END;


<*EXTERNAL*>
PROCEDURE uname (buf: struct_utsname_star): int;

END Uutsname.
