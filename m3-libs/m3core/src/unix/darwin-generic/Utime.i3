(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Fri Apr 30 14:46:35 PDT 1993 by muller    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Dec  2 11:29:00 PST 1992 by mcjones   *)
(*      modified on Mon Apr 23 16:37:40 1990 by jerome        *)
(* ow 03.10.1994 *)


INTERFACE Utime;

IMPORT Utypes;
FROM Ctypes
IMPORT char_star, int, long, unsigned_short, short, const_char_star;
FROM Utypes IMPORT clock_t, suseconds_t;

TYPE time_t = Utypes.time_t;

(*** <sys/time.h> ***)

(*
 * Structure returned by gettimeofday(2) system call,
 * and used in other calls.
 *)
TYPE
  struct_timeval = RECORD
    tv_sec: time_t;			 (* seconds *)
    tv_usec: suseconds_t;		 (* and microseconds *)
  END;
  struct_timeval_star = UNTRACED REF struct_timeval;

(*
 * Structure used as a parameter by getitimer(2) and setitimer(2) system
 * calls.
 *)
TYPE
  struct_itimerval = RECORD
    it_interval: struct_timeval;	 (* timer interval *)
    it_value: struct_timeval;		 (* current value *)
  END;

(*
 * Names of the interval timers, and structure
 * defining a timer setting.
 *)
CONST (* which *)
  ITIMER_REAL =    0;   (* real time intervals *)
  ITIMER_VIRTUAL = 1;   (* virtual time intervals *)
  ITIMER_PROF    = 2;   (* user and system virtual time *)

(*
 * Structure defined by POSIX.4 to be like a timeval.
 *)
TYPE
  struct_timespec = RECORD
    tv_sec:  time_t;			 (* seconds *)
    tv_nsec: long;			 (* and nanoseconds *)
  END;

TYPE
  struct_timezone = RECORD
    tz_minuteswest:  int;		 (* minutes west of Greenwich *)
    tz_dsttime:      int;		 (* type of dst correction *)
  END;
  struct_timezone_star = UNTRACED REF struct_timezone;

CONST
  DST_NONE = 0;  (* not on dst *)
  DST_USA  = 1;  (* USA style dst *)
  DST_AUST = 2;  (* Australian style dst *)
  DST_WET  = 3;  (* Western European dst *)
  DST_MET  = 4;  (* Middle European dst *)
  DST_EET  = 5;  (* Eastern European dst *)
  DST_CAN  = 6;  (* Canada *)

(*
 * Getkerninfo clock information structure
 *)
TYPE
  struct_clockinfo = RECORD
    hz: int;				 (* clock frequency *)
    tick: int;				 (* micro-seconds per hz tick *)
    tickadj: int;			 (* clock skew rate for adjtime() *)
    stathz: int;			 (* statistics clock frequency *)
    profhz: int;			 (* profiling clock frequency *)
  END;

<*EXTERNAL*> PROCEDURE adjtime (READONLY delta: struct_timeval;
                                VAR olddelta: struct_timeval): int;
<*EXTERNAL*> PROCEDURE futimes (fd: int; READONLY times: struct_timeval): int;
<*EXTERNAL*> PROCEDURE settimeofday (READONLY t: struct_timeval;
                                     z: struct_timezone_star := NIL): int;
<*EXTERNAL*> PROCEDURE gettimeofday (VAR t: struct_timeval;
                                     z: struct_timezone_star := NIL): int;
<*EXTERNAL*> PROCEDURE getitimer (which: int;
                                  VAR value: struct_itimerval): int;
<*EXTERNAL*> PROCEDURE setitimer (which: int;
                                  VAR value, ovalue: struct_itimerval): int;
<*EXTERNAL*> PROCEDURE utimes (path: const_char_star;
                               READONLY times: struct_timeval): int;


(*** <sys/times.h> ***)

(*
 * [XSI] Structure whose address is passed as the first parameter to times()
 *)
TYPE
  struct_tms = RECORD
    tms_utime: clock_t;			 (* [XSI] User CPU time *)
    tms_stime: clock_t;			 (* [XSI] System CPU time *)
    tms_cutime: clock_t;	 (* [XSI] Terminated children user CPU time *)
    tms_cstime: clock_t;       (* [XSI] Terminated children System CPU time *)
  END;
  struct_tms_star = UNTRACED REF struct_tms;

<*EXTERNAL*> PROCEDURE times (buffer: struct_tms_star): clock_t;

(*** <sys/timeb.h> ***)

(*
 * [XSI] Structure whose address is passed as the first parameter to ftime()
 *)
TYPE
  struct_timeb = RECORD
    time: time_t;			 (* [XSI] Seconds since the Epoch *)
    millitm: unsigned_short;	      (* [XSI] Milliseconds since the Epoch *)
    timezone: short;			 (* [XSI] Minutes west of CUT *)
    dstflag: short;			 (* [XSI] non-zero if DST in effect *)
  END;
  struct_timeb_star = UNTRACED REF struct_timeb;

<*EXTERNAL*> PROCEDURE ftime (tp: struct_timeb_star): int;

(*** <time.h> ***)
TYPE
  struct_tm = RECORD
    tm_sec:    int;			 (* seconds after the minute [0-60] *)
    tm_min:    int;			 (* minutes after the hour [0-59] *)
    tm_hour:   int;			 (* hours since midnight [0-23] *)
    tm_mday:   int;			 (* day of the month [1-31] *)
    tm_mon:    int;			 (* months since January [0-11] *)
    tm_year:   int;			 (* years since 1900 *)
    tm_wday:   int;			 (* days since Sunday [0-6] *)
    tm_yday:   int;			 (* days since January 1 [0-365] *)
    tm_isdst:  int;			 (* Daylight Savings Time flag *)
    tm_gmtoff: long;			 (* offset from CUT in seconds *)
    tm_zone:   char_star;		 (* timezone abbreviation *)
  END;
  struct_tm_star = UNTRACED REF struct_tm;

<*EXTERNAL*> PROCEDURE asctime (READONLY tm: struct_tm): char_star;
<*EXTERNAL*> PROCEDURE clock (): clock_t;
<*EXTERNAL*> PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL*> PROCEDURE getdate (string: const_char_star): struct_tm_star;
(*
  <*EXTERNAL*> PROCEDURE gmtime (READONLY clock: time_t): struct_tm_star;
  <*EXTERNAL*> PROCEDURE localtime (READONLY clock: time_t): struct_tm_star;
*)
<*EXTERNAL*> PROCEDURE gmtime (clock: UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL*> PROCEDURE localtime (clock: UNTRACED REF time_t): struct_tm_star;
(*
  <*EXTERNAL*> PROCEDURE mktime (VAR tm: struct_tm): time_t;
*)
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;
<*EXTERNAL*> PROCEDURE time (tloc: UNTRACED REF time_t := NIL): time_t;
<*EXTERNAL*> PROCEDURE tzset ();

<*EXTERNAL*> PROCEDURE nanosleep (READONLY rqtp: struct_timespec;
                                  VAR rmtp: struct_timespec): int;

END Utime.
