(* This file was generated from ../Common/Uutsname.i3.cpp. Do not edit it. *)

INTERFACE Uutsname;

FROM Ctypes IMPORT char, int;

TYPE
  struct_utsname_star = UNTRACED REF struct_utsname;
  struct_utsname = BITS 3120 FOR RECORD
    sysname    : ARRAY [0..64] OF char;
    nodename   : ARRAY [0..64] OF char;
    release    : ARRAY [0..64] OF char;
    version    : ARRAY [0..64] OF char;
    machine    : ARRAY [0..64] OF char;
    domainname : ARRAY [0..64] OF char;
  END;

<*EXTERNAL*> PROCEDURE uname (buf: struct_utsname_star): int;

END Uutsname.
