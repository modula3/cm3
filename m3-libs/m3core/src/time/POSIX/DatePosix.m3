(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Oct 28 11:40:05 PDT 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DatePosix EXPORTS Date;

IMPORT Time, M3toC, Utime, TimePosix;

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

CONST Unknown = "[Unknown zone]";

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): T =
  VAR
    date : T;
    tv   : Utime.struct_timeval;
    tm   : Utime.struct_tm;
  BEGIN
    tv := TimePosix.ToUtime(t);
    IF (z = NIL) OR (z^ = 0)
      THEN EVAL Utime.localtime_r (ADR(tv.tv_sec), ADR (tm));
      ELSE EVAL Utime.gmtime_r (ADR(tv.tv_sec), ADR(tm));
    END;

    date.year    := tm.tm_year + 1900;
    date.month   := VAL(tm.tm_mon, Month);
    date.day     := tm.tm_mday;
    date.hour    := tm.tm_hour;
    date.minute  := tm.tm_min;
    date.second  := tm.tm_sec;
    date.weekDay := VAL(tm.tm_wday, WeekDay);

    IF tm.tm_isdst = 0 THEN
      date.offset := Utime.timezone;
      date.zone   := M3toC.CopyStoT (Utime.tzname[0]);
    ELSIF tm.tm_isdst > 0 AND Utime.daylight # 0 THEN
      date.offset := Utime.altzone;
      date.zone   := M3toC.CopyStoT (Utime.tzname[1]);
    ELSE
      date.offset := 0;
      date.zone   := Unknown;
    END;

    RETURN date;
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR
    tm   : Utime.struct_tm;
    time : Utime.time_t;
    t    : Time.T;
  BEGIN
    tm.tm_sec    := d.second;
    tm.tm_min    := d.minute;
    tm.tm_hour   := d.hour;
    tm.tm_mday   := d.day;
    tm.tm_mon    := ORD(d.month);
    tm.tm_year   := d.year - 1900;
    (* tm.tm_wday ignored *)
    tm.tm_isdst  := 0; (* tell mktime that DST is not in effect *)
    time := Utime.mktime(ADR(tm));
    IF time = -1 THEN RAISE Error END;
    t := FLOAT(time, LONGREAL);
    RETURN t;
  END ToTime;

BEGIN
  Utime.tzset (); (* initialize Utime's global variables *)
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
END DatePosix.
