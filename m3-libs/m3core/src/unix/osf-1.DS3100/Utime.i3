(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Dec 30 14:19:27 PST 1992 by mcjones   *)
(*      modified on Sun May 27 08:17:19 1990 by muller        *)
(*      modified on Mon Apr 23 16:37:40 1990 by jerome        *)



INTERFACE Utime;

FROM Ctypes IMPORT char_star, int, long, long_star, 
                   unsigned_short, short;

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


TYPE
  struct_itimerval = RECORD
    it_interval: struct_timeval;            (* timer interval *)
    it_value:    struct_timeval;            (* current value *)  END;

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
    tm_gmtoff:long;    (* offset from GMT in seconds *)
    tm_zone:  char_star; (* abbreviation of timezone name *)
  END;

  struct_tm_star = UNTRACED REF struct_tm;

  time_t = int; (* seconds since the Epoch *)

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


(*** <sys/timeb.h> ***)

(*
 * Structure returned by ftime system call
 *)

TYPE
  struct_timeb = RECORD
    time:	long;
    millitm:	unsigned_short;
    timezone:	short;
    dstflag:	short;
  END;

  struct_timeb_star = UNTRACED REF struct_timeb;



(*** gettimeofday(2), settimeofday(2) - get/set date and time ***)

<*EXTERNAL*>
PROCEDURE gettimeofday (VAR t: struct_timeval;
                        VAR z: struct_timezone): int;

<*EXTERNAL*>
PROCEDURE settimeofday (VAR t: struct_timeval;
                        VAR z: struct_timezone): int;


(*** adjtime(2) - correct the time to allow synchronization of the 
                  system clock ***)

<*EXTERNAL*>
PROCEDURE adjtime (VAR delta, oldDelta: struct_timeval): int;


(*** getitimer(2), setitimer(2) - get/set value of interval timer ***)

CONST (* which *)
  ITIMER_REAL =    0;   (* real time intervals *)
  ITIMER_VIRTUAL = 1;   (* virtual time intervals *)
  ITIMER_PROF    = 2;   (* user and system virtual time *)

<*EXTERNAL*>
PROCEDURE getitimer (which: int; VAR value: struct_itimerval): int;

<*EXTERNAL*>
PROCEDURE setitimer (which: int; 
                     VAR value, ovalue: struct_itimerval): int;

(*** stime(2) - set time ***)

<*EXTERNAL*> PROCEDURE stime (VAR tp: long): int;



(*** clock(3) - report CPU time used (in micro-seconds) ***)

<*EXTERNAL*> PROCEDURE clock (): long;

(*** times(3) - get process times (in ticks) ***)

<*EXTERNAL*> PROCEDURE times (buffer: struct_tms_star): long;

(*** time(3), ftime(3) - get date and time (in seconds) ***)

<*EXTERNAL*> PROCEDURE time  (tloc: long_star): long;
<*EXTERNAL*> PROCEDURE ftime (tp: struct_timeb_star);

(*** ctime(3), localtime(3), gmtime(3), asctime(3)
     - convert date and time (in seconds)  to string ***)

<*EXTERNAL*> PROCEDURE ctime     (clock: long_star): char_star;
<*EXTERNAL*> PROCEDURE localtime (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime    (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE asctime   (tm: struct_tm_star): char_star;

(*** mktime(3) - convert a struct_tm to a time_t ***)
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;

END Utime.
