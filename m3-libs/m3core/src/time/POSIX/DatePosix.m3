(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Oct 28 11:40:05 PDT 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DatePosix EXPORTS Date;
IMPORT Time, Date, DatePosix, Utime;

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

CONST Unknown = "[Unknown zone]";
CONST GMT = "GMT";

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): Date.T =
  VAR d: DatePosix.T;
  BEGIN
    DatePosix.FromTime(t, z, d, Unknown, GMT);
    RETURN Date.T{day     := d.day,
                  hour    := d.hour,
                  minute  := d.minute,
                  month   := VAL(d.month, Month),
                  offset  := d.offset,
                  second  := d.second,
                  weekDay := VAL(d.weekDay, WeekDay),
                  year    := d.year,
                  zone    := d.zone};
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR t: Time.T;
  BEGIN
    t := DatePosix.ToTime(DatePosix.T{day     := d.day,
                                      hour    := d.hour,
                                      minute  := d.minute,
                                      month   := ORD(d.month),
                                      offset  := d.offset,
                                      second  := d.second,
                                      weekDay := ORD(d.weekDay),
                                      year    := d.year,
                                      zone    := d.zone});
    IF t = -1.0d0 THEN RAISE Error END;
    RETURN t;
  END ToTime;

BEGIN
  Utime.tzset (); (* initialize Utime's global variables *)
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
  DatePosix.TypeCheck(DatePosix.T{year := 1, month := 2, day := 3, hour := 4,
                      minute := 5, second := 6, offset := 7,
                      zone := LOOPHOLE(8, TEXT),
                      weekDay := 9}, BYTESIZE(DatePosix.T));
END DatePosix.
