(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:38 PST 1993 by kalsow  *)
(*      modified on Thu Jan 28 10:00:32 PST 1993 by mjordan *)

INTERFACE TimePosix;

IMPORT Time, Utime;

(* Conversions between a "Time.T" and a "Utime.struct_timeval". *)

PROCEDURE ToUtime(n: Time.T): Utime.struct_timeval;

PROCEDURE FromUtime(READONLY tv: Utime.struct_timeval): Time.T;

END TimePosix.
