(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 29 17:07:24 PST 1993 by mcjones *)

(* "FmtTime" is an interface for formatting dates and times like
   the default format of the Unix date(1) command. *)

INTERFACE FmtTime;

IMPORT Date, Time;

PROCEDURE Long(t: Time.T; z: Date.TimeZone := NIL): TEXT;
(* Equivalent to "RETURN DateLong(Date.FromTime(t, z))". *)

PROCEDURE Short(t: Time.T; z: Date.TimeZone := NIL): TEXT;
(* Equivalent to "RETURN DateShort(Date.FromTime(t, z))". *)

PROCEDURE DateLong(READONLY d: Date.T): TEXT;
(* Return a string of the form: "DDD MMM dd hh:mm:ss zzz yyyy". *)

PROCEDURE DateShort(READONLY d: Date.T): TEXT;
(* Return a string of the form: "MMM dd hh:mm". *)

(* If "d" is a date, the components of the values returned by
   "Long(d)" and "Short(d)" are defined as follows:

| DDD `is` WeekDay[d.weekDay]
| MMM `is` Month[d.month]
| dd  `is` Fmt.Pad(Fmt.Int(d.day), 2)
| hh  `is` Fmt.Pad(Fmt.Int(d.hour), 2, '0')
| mm  `is` Fmt.Pad(Fmt.Int(d.minute), 2, '0')
| ss  `is` Fmt.Pad(Fmt.Int(d.second), 2, '0')
| zz  `is` d.zone
| yyyy `is` Fmt.Pad(Fmt.Int(d.year), 4)

*)

CONST
  Month = ARRAY Date.Month OF TEXT {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
  WeekDay = ARRAY Date.WeekDay OF TEXT {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

END FmtTime.

(* To print the current date and time, as observed in the local time
   zone, write

|  IO.Put("The time is " & FmtTime.Long(Time.Now()) & "\n")

*)
