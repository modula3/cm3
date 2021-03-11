(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Fri Apr 30 14:46:35 PDT 1993 by muller    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Dec  2 11:29:00 PST 1992 by mcjones   *)
(*      modified on Mon Apr 23 16:37:40 1990 by jerome        *)
(* ow 03.10.1994 *)

(* hacked for Cygwin basic support
   $Id$ 
*)


INTERFACE Utime;

FROM Ctypes IMPORT char_star, int, long, long_star;

(*** <sys/time.h> ***)


TYPE
  struct_timeval = RECORD
    tv_sec: long;          (* seconds *)
    tv_usec: long;         (* and microseconds *) END;

TYPE
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

  time_t = int;

<*EXTERNAL*> PROCEDURE ctime     (clock: long_star): char_star;
<*EXTERNAL*> PROCEDURE asctime   (tm: struct_tm_star): char_star;

<*EXTERNAL*> PROCEDURE localtime (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime    (clock: long_star): struct_tm_star;

(*** mktime(3) - convert a struct_tm to a time_t ***)
<*EXTERNAL*> PROCEDURE mktime (tm: struct_tm_star): time_t;

END Utime.
