(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu May 15 13:56:58 PDT 1997 by heydon  *)
(*      modified on Mon Oct 31 07:36:44 PST 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DateLinux EXPORTS Date;

IMPORT Thread, Time, Utime, TimePosix, M3toC;

VAR mu := NEW(Thread.Mutex);
(* Protect the global storage used by "Utime.localtime" and "Utime.gmtime"
   (see the localtime(3) manpage). *)

(* Note: On Linux, timezone values are positive for zones WEST of GMT,
   and negative for zones EAST of GMT. This agrees with the convention
   for "Date.T.offset" values as defined in the "Date" interface. *)

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

CONST Unknown = "[Unknown zone]";

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
        IF tm.tm_isdst = 0 THEN
          date.offset := Utime.timezone;
          date.zone   := M3toC.CopyStoT (Utime.tzname[0]);
        ELSIF tm.tm_isdst > 0 AND Utime.daylight # 0 THEN
          date.offset := Utime.altzone;
          date.zone   := M3toC.CopyStoT (Utime.tzname[1]);
        ELSE
          date.offset := 0;
          date.zone   := Unknown;
        END
      ELSIF z^ = UTC^ THEN
        tm := Utime.gmtime(ADR(tv.tv_sec));
        date.offset := 0;
        date.zone := "UTC";
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
    END;
    RETURN date;
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR
    tm: Utime.struct_tm;
    time, now: Utime.time_t;
    local_now: Utime.struct_tm_star;
    t: Time.T;
  BEGIN
    LOCK mu DO
      (* convert to a "Utime.time_t" *)
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
      (* call localtime(3) to set "Utime.timezone" global *)
      EVAL Utime.time(ADR(now));
      local_now := Utime.localtime(ADR(now));
      (* In the following difference, we use the unshifted timezone (i.e.,
         the one that doesn't account for daylight savings) because we
         passed a value of 0 for "tm_isdst" to the mktime(3) call above. *)
      INC(time, d.offset - Utime.timezone)
    END;

    (* convert to a "Time.T" *)
    t := FLOAT(time, LONGREAL);
    RETURN t;
  END ToTime;

BEGIN
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
END DateLinux.
