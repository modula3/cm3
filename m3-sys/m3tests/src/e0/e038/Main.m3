(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*************************
Return-Path: <mhb@src.dec.com>
Received: by jumbo.pa.dec.com; id AA21809; Mon, 30 Dec 91 08:02:35 -0800
From: mhb (Marc H. Brown)
Message-Id: <9112301602.AA21809@jumbo.pa.dec.com>
Date: Mon, 30 Dec 91 08:02:25 PST
To: kalsow
X-Folder-Carbon: mhbcc
Subject: m3compile crash: 


missing object -> compile AsciiUtil.m3
"AsciiUtil.m3", line 12: undefined (Alerted)
"AsciiUtil.m3", line 18: warning: potentially unhandled exception (Thread.Alerted)
M3 runtime error: Segmentation violation - possible attempt to dereference NIL

Fatal Error: program "/proj/m3/lib.mips/m3compiler" got fatal signal 3

*** Error code 255

Stop.


core file is /flimflam/udir/mhb/m3packages/testpixmaps/core-for-kalsow;
code is 
*********************************)


(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Mon Dec 30 14:19:06 PST 1991 by kalsow     *)
(*      modified on Mon Dec 30  8:00:12 PST 1991 by mhb        *)

MODULE AsciiUtil EXPORTS Main;

IMPORT Rd, Scan, Text, Thread;
         
<* FATAL Rd.Failure *>

PROCEDURE StripPrefix (rd: Rd.T; prefix: Text.T): Text.T
  RAISES {Alerted, Error} =
  VAR
    prefixLen : INTEGER;
    line, head: Text.T;
  BEGIN
    TRY
      line := Rd.GetLine(rd)
    EXCEPT
    | Rd.EndOfFile => RAISE Error
    END;
    prefixLen := Text.Length(prefix);
    head := Text.Sub(line, 0, prefixLen);
    IF NOT Text.Equal(head, prefix) THEN RAISE Error END;
    RETURN Text.Sub(line, prefixLen, LAST(CARDINAL));
  END StripPrefix;

PROCEDURE ScanLine (rd: Rd.T): Text.T RAISES {Thread.Alerted} =
  BEGIN
    RETURN StripPrefix(rd, "")
  END ScanLine;
  
PROCEDURE ScanRInt (rd: Rd.T): INTEGER
  RAISES {Thread.Alerted, Scan.BadFormat, Error} =
  VAR
    tail  := StripPrefix(rd, "");
    index := Text.FindCharR(tail, ' ');
  BEGIN
    RETURN Scan.Int(Text.Sub(tail, index + 1, LAST(CARDINAL)))
  END ScanRInt;

PROCEDURE GetReal (rd: Rd.T; prefix: Text.T; VAR val: REAL)
  RAISES {Thread.Alerted, Scan.BadFormat, Error} =
  VAR tail := StripPrefix(rd, prefix);
  BEGIN
    val := Scan.Real(tail)
  END GetReal;

PROCEDURE GetInt (rd: Rd.T; prefix: Text.T; VAR val: INTEGER)
  RAISES {Thread.Alerted, Scan.BadFormat, Error} =
  VAR tail := StripPrefix(rd, prefix);
  BEGIN
    val := Scan.Int(tail)
  END GetInt;

PROCEDURE Get4Int (rd: Rd.T; prefix: Text.T; VAR v1, v2, v3, v4: INTEGER)
  RAISES {Thread.Alerted, Scan.BadFormat, Error} =
  VAR tail := StripPrefix(rd, prefix);
  BEGIN
    v1 := Scan.Int(tail);
    FOR i := 2 TO 4 DO
      tail := Text.Sub(tail, 1 + Text.FindChar(tail, ' '), LAST(CARDINAL));
      IF i = 2 THEN
        v2 := Scan.Int(tail);
      ELSIF i = 3 THEN
        v3 := Scan.Int(tail)
      ELSE
        v4 := Scan.Int(tail)
      END
    END
  END Get4Int;

PROCEDURE GetText (rd: Rd.T; prefix: Text.T; VAR val: Text.T)
  RAISES {Thread.Alerted, Error} =
  BEGIN
    val := StripPrefix(rd, prefix)
  END GetText;

BEGIN
END AsciiUtil.

