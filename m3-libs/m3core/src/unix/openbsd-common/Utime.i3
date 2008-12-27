(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utime;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, long;
FROM Utypes IMPORT int32_t;

CONST
  ITIMER_REAL = 0;
  ITIMER_VIRTUAL = 1;

TYPE
  struct_timeval = RECORD
    tv_sec: long;
    tv_usec: long;
  END;

  struct_timezone = RECORD
    tz_minuteswest:  int32_t;
    tz_dsttime:      int32_t;
  END;

  struct_timespec = RECORD
    tv_sec: time_t;
    tv_nsec: long;
  END;

  struct_itimerval = RECORD
    it_interval: struct_timeval;
    it_value: struct_timeval;
  END;

  struct_tm_star = UNTRACED REF struct_tm;
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
    tm_gmtoff:long;
    tm_zone:  char_star;
  END;

  time_t = Utypes.time_t;

<*EXTERNAL*> PROCEDURE gettimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int32_t;
<*EXTERNAL*> PROCEDURE settimeofday (VAR t: (*const*) struct_timeval; z: UNTRACED REF (*const*) struct_timezone := NIL): int32_t;
<*EXTERNAL*> PROCEDURE getitimer (which: int32_t; VAR value: struct_itimerval): int32_t;

<*EXTERNAL*> PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
<*EXTERNAL*> PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL*> PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;

<*EXTERNAL*> PROCEDURE setitimer (which: int32_t;  VAR (*const*) value, ovalue: struct_itimerval): int32_t;
<*EXTERNAL*> PROCEDURE nanosleep (READONLY req: struct_timespec; VAR rem: struct_timespec): int32_t;

END Utime.
