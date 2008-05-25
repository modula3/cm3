(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utime;

FROM Ctypes IMPORT char_star, int, long, long_star, const_char_star;

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
    tv_nsec: long;
  END;

TYPE
  struct_itimerval = RECORD
    it_interval: struct_timeval;
    it_value:    struct_timeval;
  END;

  struct_tm_star = UNTRACED REF struct_tm;
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

  time_t = int;

CONST
  ITIMER_REAL =    0;
  ITIMER_VIRTUAL = 1;

<*EXTERNAL*> PROCEDURE gettimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int;
<*EXTERNAL*> PROCEDURE settimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int;
<*EXTERNAL*> PROCEDURE getitimer (which: int; VAR value: struct_itimerval): int;
<*EXTERNAL setitimer*> PROCEDURE setitimer_ (which: int;  VAR value, ovalue: struct_itimerval): int;
PROCEDURE setitimer (which: int; VAR value, ovalue: struct_itimerval): int;
<*EXTERNAL*> PROCEDURE clock (): long;
<*EXTERNAL*> PROCEDURE time  (tloc: long_star): long;
<*EXTERNAL*> PROCEDURE ctime     (READONLY clock: long): char_star;
<*EXTERNAL*> PROCEDURE localtime (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime    (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;
<*EXTERNAL*> PROCEDURE get_timezone(): time_t;
<*EXTERNAL "get_timezone"*> PROCEDURE get_altzone(): time_t;
<*EXTERNAL*> PROCEDURE get_daylight(): int;
<*EXTERNAL*> PROCEDURE get_tzname(a: [0..1]): const_char_star;
<*EXTERNAL*> PROCEDURE tzset();

END Utime.
