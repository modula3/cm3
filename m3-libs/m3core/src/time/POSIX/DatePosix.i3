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
 * (They are all the same size.)
 *)
TYPE T = RECORD
  day:      INTEGER := 0;
  gmt:      INTEGER := 0; (* boolean *)
  hour:     INTEGER := 0;
  minute:   INTEGER := 0;
  month:    INTEGER := 0;
  offset:   INTEGER := 0;
  second:   INTEGER := 0;
  unknown:  INTEGER := 0; (* boolean *)
  weekDay:  INTEGER := 0;
  year:     INTEGER := 0;
  zone:     Ctypes.const_char_star := NIL;
END;

<*EXTERNAL DatePosix__FromTime*>
PROCEDURE FromTime(time: LONGREAL; zone: INTEGER; VAR date: T);

<*EXTERNAL DatePosix__ToTime*>
PROCEDURE ToTime(READONLY d: T): Time.T;

<*EXTERNAL DatePosix__TypeCheck*>
PROCEDURE TypeCheck(READONLY d: T; sizeofT: INTEGER);

END DatePosix.
