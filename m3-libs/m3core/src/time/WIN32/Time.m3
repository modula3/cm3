(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Aug 31 09:42:51 PDT 1994 by kalsow     *)
(*      modified on Wed Sep 22 14:53:33 PDT 1993 by steveg     *)
(*      modified on Thu Mar 11 13:01:04 PST 1993 by mjordan    *)

MODULE Time;

IMPORT WinBase;
IMPORT TimeWin32;

PROCEDURE Now(): T=
  VAR
    fileTime: WinBase.FILETIME;
  BEGIN
    WinBase.GetSystemTimeAsFileTime(fileTime);
    RETURN TimeWin32.FromFileTime(fileTime);
  END Now;

BEGIN
VAR t0, t1: T;
BEGIN
  (* Determine value of "Grain" experimentally.  Note that
     this will fail if this thread is descheduled for a tick during the
     loop below. *)
  (* Grain := 0.0075D0
     SCG - 21 Sep. 93 - This does have problems, use experimental value
     From WindowsNT 3.1 (final) release running on DEC 466ST (486 based)
     SCG - 22 Sep. 93 - Turns out the problem was in Now() - try again
  *)
  t0 := Now();
  REPEAT t1 := Now() UNTIL t1 # t0;
  Grain := t1-t0
END;
END Time.
