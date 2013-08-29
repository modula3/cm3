(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Aug 31 09:42:51 PDT 1994 by kalsow     *)
(*      modified on Wed Sep 22 14:53:33 PDT 1993 by steveg     *)
(*      modified on Thu Mar 11 13:01:04 PST 1993 by mjordan    *)

UNSAFE MODULE TimeWin32;

IMPORT WinBase, Word;
IMPORT Time;

CONST
  CONST LAST_INTEGER32 = 16_7FFFFFFF;
  H = FLOAT(LAST_INTEGER32, LONGREAL) + 1.0d0;
  H2 = 2.0d0 * H;
  I = Word.Plus(LAST_INTEGER32, 1);

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

BEGIN
END TimeWin32.
