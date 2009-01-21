(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* copied from openbsd-common *)

INTERFACE Usysdep;

FROM Ctypes IMPORT int, char_star;
FROM Cstdint IMPORT uint32_t, int32_t;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;

  MAX_FDSET = 1024;

TYPE
  mode_t = uint32_t;

(* INTERFACE Upthread; *)

  pthread_t = ADDRESS;
  pthread_attr_t = ADDRESS;
  pthread_mutex_t = ADDRESS;
  pthread_cond_t = ADDRESS;
  pthread_key_t = int;

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
  clock_t = int32_t;

  gid_t = uint32_t;

  pid_t = int32_t;

  (* ideally always 64 bits *)
  time_t = INTEGER;

  uid_t = uint32_t;

  socklen_t = uint32_t;
  hostent_addrtype_t = int32_t;
  hostent_length_t = int32_t;

END Usysdep.
