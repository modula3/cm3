(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Utime;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, const_char_star, int;
FROM Cstdint IMPORT int32_t;
IMPORT Usysdep;

(*CONST*)
<*EXTERNAL "Utime__ITIMER_VIRTUAL"*>
VAR ITIMER_VIRTUAL: int; (* virtual time intervals *)

TYPE

  struct_timeval = RECORD
  (* somewhat idealized; this does not necessarily match
  the underlying system; ideally we'd use LONGINT here. *)
    tv_sec: INTEGER;
    tv_usec: INTEGER;
  END;

  struct_timezone = RECORD
  (* Every system defines this the same, and we assert it in UnixC.c *)
    tz_minuteswest: int32_t; (* minutes west of Greenwich *)
    tz_dsttime:     int32_t; (* type of dst correction *)
  END;

  struct_timespec = RECORD
  (* somewhat idealized; this does not necessarily match
  the underlying system; ideally we'd use LONGINT here. *)
    tv_sec: INTEGER; (* seconds *)
    tv_nsec: INTEGER; (* nanoseconds *)
  END;

  struct_itimerval = RECORD
  (* Every system defines this the same, and we assert it in UnixC.c *)
    it_interval: struct_timeval;	 (* timer interval *)
    it_value: struct_timeval;		 (* current value *)
  END;

  struct_tm_star = UNTRACED REF struct_tm;
  struct_tm = Usysdep.struct_tm;

  time_t = Utypes.time_t;

<*EXTERNAL "Utime__gettimeofday"*>PROCEDURE gettimeofday (VAR t: struct_timeval): int32_t;
<*EXTERNAL "Utime__getitimer"*>PROCEDURE getitimer (which: int32_t; VAR value: struct_itimerval): int32_t;

<*EXTERNAL "Utime__time"*>PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
<*EXTERNAL "Utime__mktime"*>PROCEDURE mktime (tm: struct_tm_star): time_t;

<*EXTERNAL "Utime__ctime"*>PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL "Utime__localtime"*>PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL "Utime__gmtime"*>PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;

<*EXTERNAL "Utime__localtime_r"*>PROCEDURE localtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;
<*EXTERNAL "Utime__gmtime_r"*>PROCEDURE gmtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;

<*EXTERNAL "Utime__setitimer"*>PROCEDURE setitimer (which: int32_t; VAR (*const*) new_value, old_value: struct_itimerval): int32_t;
<*EXTERNAL "Utime__nanosleep"*>PROCEDURE nanosleep (READONLY req: struct_timespec; VAR rem: struct_timespec): int32_t;

<*EXTERNAL "Utime__get_timezone"*>PROCEDURE get_timezone(): time_t;
<*EXTERNAL "Utime__get_altzone"*>PROCEDURE get_altzone(): time_t;
<*EXTERNAL "Utime__get_daylight"*>PROCEDURE get_daylight(): int32_t;
<*EXTERNAL "Utime__get_tzname"*>PROCEDURE get_tzname(a: [0..1]): const_char_star;

<*EXTERNAL "Utime__tzset"*>PROCEDURE tzset();

END Utime.
