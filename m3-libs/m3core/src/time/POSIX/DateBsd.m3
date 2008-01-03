(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu May 15 13:54:02 PDT 1997 by heydon  *)
(*      modified on Fri Oct 28 14:12:53 PDT 1994 by kalsow  *)
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
      IF (z = NIL) OR (z^ = Local^) THEN
        tm := Utime.localtime(ADR(tv.tv_sec));
      ELSIF z^ = UTC^ THEN
        tm := Utime.gmtime(ADR(tv.tv_sec));
      ELSE
        (* unknown timezone *)
        <* ASSERT FALSE *>
      END;
      date.year    := tm.tm_year + 1900;
      date.month   := VAL(tm.tm_mon, Month);
      date.day     := tm.tm_mday;
      date.hour    := tm.tm_hour;
      date.minute  := tm.tm_min;
      date.second  := tm.tm_sec;
      date.weekDay := VAL(tm.tm_wday, WeekDay);
      (* The "tm.tm_gmtoff" field is seconds *east* of GMT, whereas
         the "date.offset" field is seconds *west* of GMT, so a
         negation is necessary. *)
      date.offset  := - (tm.tm_gmtoff);
      date.zone    := M3toC.CopyStoT (tm.tm_zone);
    END;
    RETURN date;
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
(* This function uses mktime(3) to convert a "Utime.struct_tm" into a
   "Utime.time_t", which is then converted via "FLOAT" to a "Time.T".
   The mktime(3) function ignores the "tm_gmtoff" field of its argument,
   so we account for the timezone information "d.offset" by determining
   the offset of any local time and adjusting the result by the difference
   between "d.offset" and the local offset. *)
  CONST
    SecsPerHour = 60 * 60;
  VAR
    tm: Utime.struct_tm;
    time, now: Utime.time_t;
    local_now: Utime.struct_tm_star;
    t: Time.T;
  BEGIN
    (* prepare call to mktime(3) *)
    tm.tm_sec    := d.second;
    tm.tm_min    := d.minute;
    tm.tm_hour   := d.hour;
    tm.tm_mday   := d.day;
    tm.tm_mon    := ORD(d.month);
    tm.tm_year   := d.year - 1900;
    (* tm.tm_wday ignored *)
    tm.tm_isdst  := 0; (* tell mktime that DST is not in effect *)
    (* tm_zone, tm_gmtoff ignored *)
    time := Utime.mktime(ADR(tm));
    IF time = -1 THEN RAISE Error END;

    (* adjust result to reflect "d.offset" *)
    EVAL Utime.time(ADR(now));
    local_now := Utime.localtime(ADR(now));
    IF local_now.tm_isdst > 0 THEN
      (* decrement the local time zone by one hour if DST is in effect *)
      DEC(local_now.tm_gmtoff, 1 * SecsPerHour)
    END;
    (* As above, we must negate "d.offset" to account for the
       opposite sense of that field compared to Unix. *)
    DEC(time, (-d.offset) - local_now.tm_gmtoff);

    (* convert to a "Time.T" *)
    t := FLOAT(time, LONGREAL);
    RETURN t;
  END ToTime;

BEGIN
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
END DateBsd.
