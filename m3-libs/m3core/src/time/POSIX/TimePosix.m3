(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:51 PST 1993 by kalsow  *)
(*      modified on Fri Apr 30 17:08:11 PDT 1993 by mjordan *)
(*      modified on Tue Jan 12 13:09:41 PST 1993 by mcjones *)

MODULE TimePosix EXPORTS Time, TimePosix;

IMPORT Utime;

PROCEDURE Now(): T =
  VAR
    tv: Utime.struct_timeval;
    tz: Utime.struct_timezone;
    i := Utime.gettimeofday(tv, tz);
  BEGIN
    <* ASSERT i=0 *>
    RETURN FromUtime(tv);
  END Now;

PROCEDURE ToUtime(n: T): Utime.struct_timeval=
  VAR tv: Utime.struct_timeval;
  BEGIN
    tv.tv_sec := TRUNC(n);
    tv.tv_usec := ROUND((n - FLOAT(tv.tv_sec, LONGREAL)) * 1.0D6);
    RETURN tv;
  END ToUtime;    

PROCEDURE FromUtime(READONLY tv: Utime.struct_timeval): T=
  BEGIN
    RETURN FLOAT(tv.tv_sec, LONGREAL) + FLOAT(tv.tv_usec, LONGREAL) / 1.0D6
  END FromUtime;    

VAR t0, t1: T;
BEGIN
  (* Determine value of "Grain" experimentally.  Note that
     this will fail if this thread is descheduled for a tick during the
     loop below. *)
  t0 := Now();
  REPEAT t1 := Now() UNTIL t1 # t0;
  Grain := t1-t0
END TimePosix.
