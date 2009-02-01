(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Utime;

FROM Ctypes IMPORT char_star, const_char_star, int;
FROM Cstdint IMPORT int32_t;
IMPORT Usysdep;

(*CONST*)
<*EXTERNAL "Utime_ITIMER_VIRTUAL"*> VAR ITIMER_VIRTUAL: int; (* virtual time intervals *)

TYPE
  struct_timeval = Usysdep.struct_timeval;

  struct_timezone = RECORD
    tz_minuteswest: int32_t; (* minutes west of Greenwich *)
    tz_dsttime:     int32_t; (* type of dst correction *)
  END;

  struct_timespec = RECORD
    tv_sec: time_t; (* seconds *)
    tv_nsec: INTEGER; (* nanoseconds *)
  END;

  struct_itimerval = RECORD
    it_interval: struct_timeval;	 (* timer interval *)
    it_value: struct_timeval;		 (* current value *)
  END;

  struct_tm_star = UNTRACED REF struct_tm;
  struct_tm = Usysdep.struct_tm;

  time_t = Usysdep.time_t;

PROCEDURE gettimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int32_t;
PROCEDURE settimeofday (VAR t: (*const*) struct_timeval; z: UNTRACED REF (*const*) struct_timezone := NIL): int32_t;
PROCEDURE getitimer (which: int32_t; VAR value: struct_itimerval): int32_t;

PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
PROCEDURE mktime (tm: struct_tm_star): time_t;

PROCEDURE ctime (READONLY clock: time_t): char_star;
PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;

PROCEDURE ctime_r (READONLY clock: time_t; buffer: char_star): char_star;
PROCEDURE localtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;
PROCEDURE gmtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;

PROCEDURE setitimer (which: int32_t; VAR (*const*) new_value, old_value: struct_itimerval): int32_t;
PROCEDURE nanosleep (READONLY req: struct_timespec; VAR rem: struct_timespec): int32_t;

<*EXTERNAL "m3_get_timezone"*> PROCEDURE get_timezone(): time_t;
<*EXTERNAL "m3_get_altzone"*> PROCEDURE get_altzone(): time_t;
<*EXTERNAL "m3_get_daylight"*> PROCEDURE get_daylight(): int32_t;
<*EXTERNAL "m3_get_tzname"*> PROCEDURE get_tzname(a: [0..1]): const_char_star;

PROCEDURE tzset();

END Utime.
