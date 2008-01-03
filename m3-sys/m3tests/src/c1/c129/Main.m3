(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(****
Return-Path: <grinberg@src.dec.com>
Received: by jumbo.pa.dec.com; id AA15151; Fri, 9 Aug 91 15:25:59 -0700
From: grinberg (Dennis Grinberg)
Message-Id: <9108092225.AA15151@jumbo.pa.dec.com>
Date: Fri,  9 Aug 91 15:25:48 PDT
To: kalsow
Cc: meehan
X-Folder-Carbon: hector.m3
Subject: READONLY bug

Bill,

Here's a short sample which demonstrates the problem:
******)

UNSAFE MODULE Main;
IMPORT Wr, Stdio, Fmt, Thread;

VAR
  i:[-16_8000 .. 16_7FFF];
  k: REF INTEGER;

  PROCEDURE Arf(READONLY j: INTEGER) =
    <*FATAL Wr.Failure, Thread.Alerted *>
    BEGIN
      Wr.PutText(Stdio.stderr,Fmt.F("%s",Fmt.Int(j)));
    END Arf;

BEGIN
  k := LOOPHOLE(ADR(i), REF INTEGER);
  k^ := 16_7FFFFFFF;  (* get something into those extra 2 bytes *)
  i := 3;
  Arf(i);
END Main.


