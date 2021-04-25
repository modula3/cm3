(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:38 PST 1993 by kalsow  *)
(*      modified on Thu Jan 28 10:00:32 PST 1993 by mjordan *)

INTERFACE DatePosix;
IMPORT Date, Time;

(* This MUST match m3core.h
 * The fields are ordered by size and alphabetically.
 * (They are all the same size.)
 *)
TYPE T = RECORD
  day:      INTEGER;
  hour:     INTEGER;
  minute:   INTEGER;
  month:    INTEGER;
  offset:   INTEGER;
  second:   INTEGER;
  weekDay:  INTEGER;
  year:     INTEGER;
  zone:     TEXT;
  zzalign:  INTEGER := 10;
END;

<*EXTERNAL DatePosix__FromTime*>
PROCEDURE FromTime(time: LONGREAL; zone: INTEGER; VAR date: T; unknown, gmt: TEXT);

<*EXTERNAL DatePosix__ToTime*>
PROCEDURE ToTime(READONLY d: T): Time.T;

<*EXTERNAL DatePosix__TypeCheck*>
PROCEDURE TypeCheck(READONLY d: T; sizeofT: INTEGER);

END DatePosix.
