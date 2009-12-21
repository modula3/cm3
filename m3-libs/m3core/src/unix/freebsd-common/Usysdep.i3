(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* copied from openbsd-common *)

INTERFACE Usysdep;

FROM Ctypes IMPORT int, char_star;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;
  MAX_FDSET = 1024;

TYPE
(* INTERFACE Usocket; *)

  struct_linger = RECORD
    l_onoff: int;
    l_linger: int;
  END;

(* INTERFACE Utime; *)

  struct_timeval = RECORD
    tv_sec: time_t;
    tv_usec: INTEGER;
  END;

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
    tm_gmtoff:INTEGER;
    tm_zone:  char_star;
  END;

(* INTERFACE Utypes; *)

  (* ideally always 64 bits *)
  time_t = INTEGER;

END Usysdep.
