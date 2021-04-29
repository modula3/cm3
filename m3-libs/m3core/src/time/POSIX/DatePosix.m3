(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Oct 28 11:40:05 PDT 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DatePosix EXPORTS Date;
IMPORT Ctypes, M3toC, Scheduler, Time, Date, DatePosix, Utime;

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

CONST Unknown = "[Unknown zone]";
CONST GMT = "GMT";

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): Date.T =
  VAR d: DatePosix.T;
      i: INTEGER := 0; (* default to Local *)
      zone: TEXT := NIL;
  BEGIN
    IF z # NIL THEN
      i := z^;
    END;
    Scheduler.DisableSwitching();
    DatePosix.FromTime(t, i, d);
    Scheduler.EnableSwitching();
    <* ASSERT d.gmt + d.unknown + ORD(d.zone # NIL) = 1 *>
    IF d.gmt # 0 THEN
      zone := GMT;
    ELSIF d.unknown # 0 THEN
      zone := Unknown;
    ELSE
      zone := M3toC.CopyStoT (d.zone);
    END;
    RETURN Date.T{day     := d.day,
                  hour    := d.hour,
                  minute  := d.minute,
                  month   := VAL(d.month, Month),
                  offset  := d.offset,
                  second  := d.second,
                  weekDay := VAL(d.weekDay, WeekDay),
                  year    := d.year,
                  zone    := zone};
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR t: Time.T;
  BEGIN
    Scheduler.DisableSwitching();
    t := DatePosix.ToTime(DatePosix.T{day     := d.day,
                                      hour    := d.hour,
                                      minute  := d.minute,
                                      month   := ORD(d.month),
                                      offset  := d.offset,
                                      second  := d.second,
                                      weekDay := ORD(d.weekDay),
                                      year    := d.year,
                                      zone    := NIL, (* not used *)
                                      gmt     := 0,   (* not used *)
                                      unknown := 0}); (* not used *)
    Scheduler.EnableSwitching();
    IF t = -1.0d0 THEN RAISE Error END;
    RETURN t;
  END ToTime;

BEGIN
  Scheduler.DisableSwitching();
  Utime.tzset (); (* initialize Utime's global variables *)
  Scheduler.EnableSwitching();
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;

  (* Fill in known values to each field and C asserts they are as expected. *)
  DatePosix.TypeCheck(DatePosix.T{year    := 1,
                                  month   := 2,
                                  day     := 3,
                                  hour    := 4,
                                  minute  := 5,
                                  second  := 6,
                                  offset  := 7,
                                  zone    := LOOPHOLE(8, Ctypes.char_star),
                                  weekDay := 9,
                                  gmt     := 10,
                                  unknown := 11},
                      BYTESIZE(DatePosix.T));
END DatePosix.
