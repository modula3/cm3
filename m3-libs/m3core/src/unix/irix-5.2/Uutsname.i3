(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

INTERFACE Uutsname;

FROM Ctypes IMPORT char, int;

CONST
  SYS_NMLN = 257;

TYPE
  struct_utsname_star = UNTRACED REF struct_utsname;
  struct_utsname = RECORD
    sysname   : ARRAY [0..SYS_NMLN-1] OF char;
    nodename  : ARRAY [0..SYS_NMLN-1] OF char;
    release   : ARRAY [0..SYS_NMLN-1] OF char;
    version   : ARRAY [0..SYS_NMLN-1] OF char;
    machine   : ARRAY [0..SYS_NMLN-1] OF char;
    m_type    : ARRAY [0..SYS_NMLN-1] OF char;
    base_rel  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve5  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve4  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve3  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve2  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve1  : ARRAY [0..SYS_NMLN-1] OF char;
    reserve0  : ARRAY [0..SYS_NMLN-1] OF char;
  END;


<*EXTERNAL*>
PROCEDURE uname (buf: struct_utsname_star): int;

END Uutsname.
