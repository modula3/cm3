(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Oct 28 14:12:53 PDT 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DateBsd EXPORTS Date;

IMPORT Thread, Time, M3toC, Utime, TimePosix;

VAR mu := NEW(Thread.Mutex);
(* Protect the static storage used by "Utime.localtime" and "Utime.gmtime"
   (see the localtime(3) manpage). *)

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): T =
  VAR
    date : T;
    tv   : Utime.struct_timeval;
    tm   : Utime.struct_tm_star;
  BEGIN
    tv := TimePosix.ToUtime(t);
    LOCK mu DO
      IF (z = NIL) OR (z^ = 0)
        THEN tm := Utime.localtime(ADR(tv.tv_sec));
        ELSE tm := Utime.gmtime(ADR(tv.tv_sec));
      END;
      date.year    := tm.tm_year + 1900;
      date.month   := VAL(tm.tm_mon, Month);
      date.day     := tm.tm_mday;
      date.hour    := tm.tm_hour;
      date.minute  := tm.tm_min;
      date.second  := tm.tm_sec;
      date.weekDay := VAL(tm.tm_wday, WeekDay);
      date.offset  := tm.tm_gmtoff;
      date.zone    := M3toC.CopyStoT (tm.tm_zone);
    END;
    RETURN date;
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR
    tm: Utime.struct_tm;
    time: Utime.time_t;
    t: Time.T;
  BEGIN
    tm.tm_sec    := d.second;
    tm.tm_min    := d.minute;
    tm.tm_hour   := d.hour;
    tm.tm_mday   := d.day;
    tm.tm_mon    := ORD(d.month);
    tm.tm_year   := d.year - 1900;
    (* tm.tm_wday ignored *)
    tm.tm_isdst  := 0; (* tell mktime that DST is not in effect *)
    tm.tm_gmtoff := d.offset;
    (* tm_zone ignored *)
    time := Utime.mktime(ADR(tm));
    IF time = -1 THEN RAISE Error END;
    t := FLOAT(time, LONGREAL);
    RETURN t;
  END ToTime;

BEGIN
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
END DateBsd.
