(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Cstdint IMPORT uint32_t;
FROM Ctypes IMPORT char_star, int;
IMPORT Upthreadtypes;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;

  MAX_FDSET = 1024;

TYPE
  mode_t = uint32_t;

(* INTERFACE Upthread; *)

  pthread_t = ADDRESS;
  pthread_attr_t = Upthreadtypes.pthread_attr_t;
  pthread_mutex_t = Upthreadtypes.pthread_mutex_t;
  pthread_cond_t = RECORD data: ARRAY[1..6] OF LONGINT; END;
  pthread_key_t = uint32_t;

(* INTERFACE Usocket; *)

  struct_linger = RECORD
    l_onoff: int;
    l_linger: int;
  END;

(* INTERFACE Utime; *)

  struct_timeval = RECORD
    tv_sec: INTEGER;
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

  gid_t = uint32_t;
  pid_t = int;
  time_t = INTEGER; (* ideally always 64 bits *)
  uid_t = uint32_t;

  socklen_t = uint32_t;
  hostent_addrtype_t = int;
  hostent_length_t = int;

END Usysdep.
