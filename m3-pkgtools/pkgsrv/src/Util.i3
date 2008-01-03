(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Util.i3 *)
(* Last modified on Fri Apr 23 14:31:50 PDT 1993 by wobber  *)
(*      modified on Wed Jul  8 14:23:10 GMT+2:00 1992 by prusker *)

INTERFACE Util;

IMPORT Time;

    (* miscellaneous *)

PROCEDURE Unique(): CARDINAL;

    (* printing date and duration *)

PROCEDURE Interval(t: Time.T) : TEXT;

PROCEDURE IntervalSince(t: Time.T) : TEXT;

PROCEDURE LogText(text: TEXT);
    (* write to log file *)

END Util.
