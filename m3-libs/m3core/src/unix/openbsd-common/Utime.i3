(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utime;

FROM Utypes IMPORT clock_t;
FROM Ctypes IMPORT char_star, int, long, long_int, unsigned_short, short;

CONST
  DST_NONE = 0;
  DST_USA  = 1;
  DST_AUST = 2;
  DST_WET  = 3;
  DST_MET  = 4;
  DST_EET  = 5;
  DST_CAN  = 6;
  ITIMER_REAL = 0;
  ITIMER_VIRTUAL = 1;
  ITIMER_PROF = 2;

TYPE
  struct_timeval = RECORD
    tv_sec: long;
    tv_usec: long;
 END;

  struct_timezone = RECORD
    tz_minuteswest:  int;
    tz_dsttime:      int;
  END;

  struct_timespec = RECORD
    tv_sec: time_t;
    tv_nsec: long_int;
  END;

  struct_itimerval = RECORD
    it_interval: struct_timeval;
    it_value: struct_timeval;
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
    tm_gmtoff:long;
    tm_zone:  char_star;
  END;

  struct_tm_star = UNTRACED REF struct_tm;
  time_t = int;

  struct_tms = RECORD
    tms_utime: clock_t;
    tms_stime: clock_t;
    tms_cutime: clock_t;
    tms_cstime: clock_t;
  END;
  struct_tms_star = UNTRACED REF struct_tms;

  struct_timeb = RECORD
    time:       time_t;
    millitm:    unsigned_short;
    timezone:   short;
    dstflag:    short;
  END;
  struct_timeb_star = UNTRACED REF struct_timeb;

<*EXTERNAL*> PROCEDURE gettimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int;
<*EXTERNAL*> PROCEDURE settimeofday (VAR t: (*const*) struct_timeval; z: UNTRACED REF (*const*) struct_timezone := NIL): int;
<*EXTERNAL*> PROCEDURE getitimer (which: int; VAR value: struct_itimerval): int;
<*EXTERNAL*> PROCEDURE setitimer (which: int;  VAR (*const*) value, ovalue: struct_itimerval): int;
<*EXTERNAL*> PROCEDURE clock (): clock_t;
<*EXTERNAL*> PROCEDURE times (buffer: struct_tms_star): clock_t;
<*EXTERNAL*> PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
<*EXTERNAL*> PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL*> PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;
<*EXTERNAL*> PROCEDURE nanosleep (READONLY req: struct_timespec; VAR rem: struct_timespec): int;

END Utime.
