(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Ctypes IMPORT int;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;
  MAX_FDSET = 1024; (* header says 4096 but that doesn't work and 1024 does *)

TYPE
(* INTERFACE Utime; *)
  struct_tm = RECORD
    tm_sec:   int;
    tm_min:   int;
    tm_hour:  int;
    tm_mday:  int;
    tm_mon:   int;
    tm_year:  int;
    tm_wday:  int;
    tm_yday:  int;
    tm_isdst: int;
  END;

END Usysdep.
