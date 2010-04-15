(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:38 PST 1993 by kalsow  *)
(*      modified on Thu Jan 28 10:00:32 PST 1993 by mjordan *)

INTERFACE DatePosix;
IMPORT Date, Time;

<*EXTERNAL DatePosix__FromTime*>
PROCEDURE FromTime(time: LONGREAL; zone: Date.TimeZone; VAR date: Date.T; unknown, gmt: TEXT);

<*EXTERNAL DatePosix__ToTime*>
PROCEDURE ToTime(READONLY d: Date.T): Time.T;

<*EXTERNAL DatePosix__TypeCheck*>
PROCEDURE TypeCheck(READONLY d: Date.T; sizeof_DateT: INTEGER);

END DatePosix.
