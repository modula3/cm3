(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Aug 31 09:42:51 PDT 1994 by kalsow     *)
(*      modified on Wed Sep 22 14:53:33 PDT 1993 by steveg     *)
(*      modified on Thu Mar 11 13:01:04 PST 1993 by mjordan    *)

UNSAFE MODULE TimeWin32 EXPORTS Time, TimeWin32;

IMPORT WinBase, Word;
IMPORT Time;

CONST
  H = FLOAT(LAST(INTEGER), LONGREAL) + 1.0d0;
  H2 = 2.0d0 * H;
  I = Word.Plus(LAST(INTEGER), 1);

PROCEDURE ToFileTime(n: Time.T): WinBase.FILETIME=
  VAR low: LONGREAL;
    fileTime: WinBase.FILETIME;
  BEGIN
    n := n * 1.0D7; 
    fileTime.dwHighDateTime := TRUNC(n/H2);
    low := n - FLOAT(fileTime.dwHighDateTime, LONGREAL) * H2;
    IF low >= H THEN
      fileTime.dwLowDateTime := Word.Plus(ROUND(low - H), I);
    ELSE
       fileTime.dwLowDateTime := ROUND(low)
    END;
    RETURN fileTime; 
  END ToFileTime;
  

PROCEDURE FromFileTime (fileTime: WinBase.FILETIME): Time.T =
  VAR low, high: LONGREAL;
  BEGIN
    IF fileTime.dwLowDateTime < 0 THEN
      low := H + FLOAT(Word.And(fileTime.dwLowDateTime, LAST(INTEGER)),
                       LONGREAL);
    ELSE
      low := FLOAT(fileTime.dwLowDateTime, LONGREAL);
    END;
    high := FLOAT(fileTime.dwHighDateTime, LONGREAL) * H2;
    RETURN (low + high) / 1.0D7;
  END FromFileTime;

PROCEDURE Now(): T=
  VAR
    systemTime: WinBase.SYSTEMTIME;
    fileTime: WinBase.FILETIME;
    status: INTEGER;
  BEGIN
    WinBase.GetSystemTime(ADR(systemTime));
    status := WinBase.SystemTimeToFileTime(ADR(systemTime), ADR(fileTime));
    <*ASSERT status # 0*>
    RETURN FromFileTime(fileTime);
  END Now;

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
END TimeWin32.
