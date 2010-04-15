(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Oct 28 11:40:05 PDT 1994 by kalsow  *)
(*      modified on Thu Jan 28 10:45:24 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 17:35:53 PST 1992 by mcjones *)

UNSAFE MODULE DatePosix EXPORTS Date;

IMPORT Time, DatePosix, Utime;

REVEAL TimeZone = BRANDED "Date.TimeZone" REF INTEGER;

CONST Unknown = "[Unknown zone]";
CONST GMT = "GMT";

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): T =
  VAR date: T;
  BEGIN
    DatePosix.FromTime(t, z, date, Unknown, GMT);
    RETURN date;
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES {Error} =
  VAR t: Time.T;
  BEGIN
    t := DatePosix.ToTime(d);
    IF t = -1.0d0 THEN RAISE Error END;
    RETURN t;
  END ToTime;

BEGIN
  Utime.tzset (); (* initialize Utime's global variables *)
  Local := NEW(TimeZone);  Local^ := 0;
  UTC   := NEW(TimeZone);  UTC^   := 1;
  DatePosix.TypeCheck(T{year := 1, month := Month.Mar, day := 3, hour := 4,
                      minute := 5, second := 6, offset := 7,
                      zone := LOOPHOLE(8, TEXT),
                      weekDay := WeekDay.Sat}, BYTESIZE(T));
END DatePosix.
