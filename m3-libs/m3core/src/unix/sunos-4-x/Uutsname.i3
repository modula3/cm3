(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Uutsname;

FROM Ctypes IMPORT char, int;

TYPE
  struct_utsname_star = UNTRACED REF struct_utsname;
  struct_utsname = RECORD
    sysname   : ARRAY [0..8] OF char;
    nodename  : ARRAY [0..64] OF char;
    release   : ARRAY [0..8] OF char;
    version   : ARRAY [0..8] OF char;
    machine   : ARRAY [0..8] OF char;
  END;

<*EXTERNAL*>
PROCEDURE uname (buf: struct_utsname_star): int;

END Uutsname.
