(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Oct 27 15:22:30 PDT 1994 by kalsow    *)
(*      modified on Thu Oct 20 18:37:29 PDT 1994 by ericv     *)
(*      modified on Thu Dec 10 17:08:05 PST 1992 by mcjones   *)
(*      modified on Sun May 27 08:17:19 1990 by muller        *)
(*      modified on Mon Apr 23 16:37:40 1990 by jerome        *)



INTERFACE Utime;

FROM Ctypes IMPORT char_star, int, long, double;
IMPORT Utypes;

(*** <time.h> ***)

CONST
  CLOCKS_PER_SEC	 = 1000000;

TYPE
  clock_t = Utypes.clock_t;
  time_t = Utypes.time_t;
  time_t_star = UNTRACED REF time_t;

  struct_tm = RECORD
    tm_sec:   int;     (* seconds (0 - 59) *)
    tm_min:   int;     (* minutes (0 - 59) *)
    tm_hour:  int;     (* hours (0 - 23) *)
    tm_mday:  int;     (* day of month (1 - 31) *)
    tm_mon:   int;     (* month of year (0 - 11) *)
    tm_year:  int;     (* year - 1900 *)
    tm_wday:  int;     (* day of week (Sunday = 0) *)
    tm_yday:  int;     (* day of year (0 - 365) *)
    tm_isdst: int;     (* flag: daylight savings time in effect *)
  END;
  struct_tm_star = UNTRACED REF struct_tm;

(*** clock(3) - report CPU time used (in micro-seconds) ***)
<*EXTERNAL*> PROCEDURE clock (): clock_t;

(*** difftime(3c) - compute difference between two calendar times ***)
<*EXTERNAL*> PROCEDURE difftime (time1, time0: time_t): double;

(*** mktime(3) - convert a struct_tm to a time_t ***)
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;

(*** time(2) - get time ***)
<*EXTERNAL*> PROCEDURE time (tloc: time_t_star): time_t;

(*** ctime(3), localtime(3), gmtime(3), asctime(3)
     - convert date and time (in seconds)  to string ***)

<*EXTERNAL*> PROCEDURE asctime   (tm: struct_tm_star): char_star;
<*EXTERNAL*> PROCEDURE ctime     (clock: time_t_star): char_star;
<*EXTERNAL*> PROCEDURE gmtime    (clock: time_t_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE localtime (clock: time_t_star): struct_tm_star;

(*** Re-entrant versions of the above ***)

<*EXTERNAL*>
PROCEDURE asctime_r(tm: struct_tm_star; buf: char_star; buflen: int):char_star;

<*EXTERNAL*>
PROCEDURE ctime_r(clock: time_t_star; buf: char_star; buflen: int): char_star;

<*EXTERNAL*>
PROCEDURE gmtime_r(clock: time_t_star; result: struct_tm_star): struct_tm_star;

<*EXTERNAL*>
PROCEDURE localtime_r(clock: time_t_star; result: struct_tm_star): struct_tm_star;

(*** tzset(3c) - set time zone information from environment variable TZ
               - called automatically by asctime, asctime_r ***)

<*EXTERNAL*> PROCEDURE tzset ();

<*EXTERNAL*> VAR tzname: ARRAY [0..1] OF char_star;
<*EXTERNAL*> VAR daylight: int;
<*EXTERNAL*> VAR timezone, altzone: time_t;

VAR (*CONST*) CLK_TCK: INTEGER;


(*** <sys/time.h> ***)

TYPE
  struct_timeval = RECORD
    tv_sec: long;          (* seconds *)
    tv_usec: long;         (* and microseconds *) END;

  struct_timezone = RECORD
    tz_minuteswest:  int; (* minutes west of Greenwich *)
    tz_dsttime:      int; (* type of dst correction *) END;

CONST
  DST_NONE = 0;  (* not on dst *)
  DST_USA  = 1;  (* USA style dst *)
  DST_AUST = 2;  (* Australian style dst *)
  DST_WET  = 3;  (* Western European dst *)
  DST_MET  = 4;  (* Middle European dst *)
  DST_EET  = 5;  (* Eastern European dst *)
  DST_CAN	= 6;	(* Canada *)
  DST_GB        = 7;       (* Great Britain and Eire *)
  DST_RUM       = 8;       (* Rumania *)
  DST_TUR       = 9;       (* Turkey *)
  DST_AUSTALT   = 10;      (* Australian style with shift in 1986 *)


CONST (* which *)
  ITIMER_REAL =    0;   (* real time intervals *)
  ITIMER_VIRTUAL = 1;   (* virtual time intervals *)
  ITIMER_PROF    = 2;   (* user and system virtual time *)

TYPE
  struct_itimerval = RECORD
    it_interval: struct_timeval;            (* timer interval *)
    it_value:    struct_timeval;            (* current value *)  END;

(*
 * Higher resolution time information
 *)
TYPE
  timestruc_t = struct_timestruc;
  struct_timestruc = RECORD
	tv_sec	: time_t;		(* seconds *)
        tv_nsec : long;                 (* and nanoseconds *)
  END;

(*** gettimeofday(2), settimeofday(2) - get/set date and time ***)

<*EXTERNAL BSDgettimeofday *>
PROCEDURE gettimeofday (VAR t: struct_timeval;
                        VAR z: struct_timezone): int;

<*EXTERNAL BSDsettimeofday *>
PROCEDURE settimeofday (VAR t: struct_timeval;
                        VAR z: struct_timezone): int;


(*** adjtime(2) - correct the time to allow synchronization of the 
                  system clock ***)

<*EXTERNAL*>
PROCEDURE adjtime (VAR delta, oldDelta: struct_timeval): int;


(*** getitimer(2), setitimer(2) - get/set value of interval timer ***)

<*EXTERNAL*>
PROCEDURE getitimer (which: int; VAR value: struct_itimerval): int;

<*EXTERNAL*>
PROCEDURE setitimer (which: int; 
                     VAR value, ovalue: struct_itimerval): int;


(*** <unistd.h> ***)

(*** stime(2) - set time ***)
<*EXTERNAL*> PROCEDURE stime (tp: time_t_star): int;


(*** <sys/times.h> ***)

(*
 * Structure returned by times()
 *)

TYPE
  struct_tms = RECORD
        tms_utime: long;              (* user time *)
        tms_stime: long;              (* system time *)
        tms_cutime: long;             (* user time, children *)
        tms_cstime: long;             (* system time, children *)
  END;

  struct_tms_star = UNTRACED REF struct_tms;

(*** times(3) - get process times (in ticks) ***)

<*EXTERNAL*> PROCEDURE times (buffer: struct_tms_star): clock_t;


END Utime.
