(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Oct  2 14:03:26 PDT 1992 by birrell       *)

INTERFACE UtimeExtra;

FROM Ctypes IMPORT char_star, int, long, long_star;

(* The declaration of struct_tm in Utime omits "gmtoff"; mktime isn there
   at all. *)

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
    tm_gmtoff: long;   (* offset from GMT in seconds *)
    tm_zone: char_star;(* abbreviation of timezone name *)
  END;

  struct_tm_star = UNTRACED REF struct_tm;

<*EXTERNAL*> PROCEDURE localtime (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime    (clock: long_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE mktime    (VAR tm: struct_tm): long;

END UtimeExtra.
