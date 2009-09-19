(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* copied from openbsd-common *)

INTERFACE Usysdep;

FROM Ctypes IMPORT char_star;
FROM Cstdint IMPORT int32_t;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;
  MAX_FDSET = 1024;

TYPE
(* INTERFACE Utime; *)

  struct_timeval = RECORD
    tv_sec: time_t;
    tv_usec: INTEGER;
  END;

  struct_tm = RECORD
    tm_sec:   int32_t;
    tm_min:   int32_t;
    tm_hour:  int32_t;
    tm_mday:  int32_t;
    tm_mon:   int32_t;
    tm_year:  int32_t;
    tm_wday:  int32_t;
    tm_yday:  int32_t;
    tm_isdst: int32_t;
    tm_gmtoff:INTEGER;
    tm_zone:  char_star;
  END;

(* INTERFACE Utypes; *)

  (* ideally always 64 bits *)
  time_t = INTEGER;

END Usysdep.
