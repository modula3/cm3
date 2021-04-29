(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:38 PST 1993 by kalsow  *)
(*      modified on Thu Jan 28 10:00:32 PST 1993 by mjordan *)

INTERFACE DatePosix;
IMPORT Date, Time, Ctypes;

(* This MUST match m3core.h
 * The fields are ordered by size and alphabetically.
 *)
TYPE T = RECORD
  zone:     Ctypes.const_char_star := NIL;
  day:      Ctypes.int := 0;
  gmt:      Ctypes.int := 0; (* boolean *)
  hour:     Ctypes.int := 0;
  minute:   Ctypes.int := 0;
  month:    Ctypes.int := 0;
  offset:   Ctypes.int := 0;
  second:   Ctypes.int := 0;
  unknown:  Ctypes.int := 0; (* boolean *)
  weekDay:  Ctypes.int := 0;
  year:     Ctypes.int := 0;
END;

<*EXTERNAL DatePosix__FromTime*>
PROCEDURE FromTime(time: LONGREAL; zone: INTEGER; VAR date: T);

<*EXTERNAL DatePosix__ToTime*>
PROCEDURE ToTime(READONLY d: T): Time.T;

<*EXTERNAL DatePosix__TypeCheck*>
PROCEDURE TypeCheck(READONLY d: T; sizeofT: INTEGER);

END DatePosix.
