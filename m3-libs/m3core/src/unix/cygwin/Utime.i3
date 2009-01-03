(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utime;

IMPORT Utypes;
FROM Ctypes IMPORT char_star, const_char_star;
FROM Utypes IMPORT int32_t;
IMPORT Usysdep;

CONST
  ITIMER_REAL = Usysdep.ITIMER_REAL;
  ITIMER_VIRTUAL = Usysdep.ITIMER_VIRTUAL;

TYPE
  struct_timeval = Usysdep.struct_timeval;

  struct_timezone = Usysdep.struct_timezone;

  struct_timespec = Usysdep.struct_timespec;

  struct_itimerval = Usysdep.struct_itimerval;

  struct_tm_star = UNTRACED REF struct_tm;
  struct_tm = Usysdep.struct_tm;

  time_t = Utypes.time_t;

<*EXTERNAL*> PROCEDURE gettimeofday (VAR t: struct_timeval; z: UNTRACED REF struct_timezone := NIL): int32_t;
<*EXTERNAL*> PROCEDURE settimeofday (VAR t: (*const*) struct_timeval; z: UNTRACED REF (*const*) struct_timezone := NIL): int32_t;
<*EXTERNAL*> PROCEDURE getitimer (which: int32_t; VAR value: struct_itimerval): int32_t;

<*EXTERNAL*> PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;

<*EXTERNAL*> PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL*> PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;

<*EXTERNAL*> PROCEDURE ctime_r (READONLY clock: time_t; buffer: char_star): char_star;
<*EXTERNAL*> PROCEDURE localtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;

<*EXTERNAL "m3_setitimer"*> PROCEDURE setitimer (which: int32_t; VAR value, ovalue: struct_itimerval): int32_t;

(* Why is get_altzone aliased to get_timezone? *)

<*EXTERNAL "m3_get_timezone"*> PROCEDURE get_timezone(): time_t;
<*EXTERNAL "m3_get_timezone"*> PROCEDURE get_altzone(): time_t;
<*EXTERNAL "m3_get_daylight"*> PROCEDURE get_daylight(): int32_t;
<*EXTERNAL "m3_get_tzname"*> PROCEDURE get_tzname(a: [0..1]): const_char_star;

<*EXTERNAL*> PROCEDURE tzset();

END Utime.
