(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(************************************************************
Return-Path: <harrison@kodak.pa.dec.com>
Received: by jumbo.pa.dec.com; id AA22488; Wed, 13 Nov 91 16:55:22 -0800
Received: by kodak.pa.dec.com; id AA24170; Wed, 13 Nov 91 16:55:29 -0800
Received: by gossamer.pa.dec.com; id AA23791; Wed, 13 Nov 91 16:55:28 -0800
Message-Id: <9111140055.AA23791@gossamer.pa.dec.com>
To: kalsow
Subject: This m3 program crashes the C compiler
Date: Wed, 13 Nov 91 16:55:27 -0800
From: "Stephen Harrison" <harrison>
X-Mts: smtp


Bill,

Try this with 2.0--it breaks the C compiler with 1.6.

Cheers,

/Stephen

----------------------------8<--------------------------
**************************************************************************)

MODULE Main;

(*
 * $Header: /opt/cvs/cm3/m3-sys/m3tests/src/p1/p108/Main.m3,v 1.1 2003-03-08 22:36:36 wagner Exp $
 *
 * Created on Wed Nov 13 16:23:14 1991 by Stephen Harrison
 *
 * Last Modified on Wed Nov  2 13:59:30 PST 1994 by kalsow  
 *      Modified on Wed Nov 13 16:52:16 1991 by harrison
 *
 * Copyright 1991 Digital Equipment Corporation.
 * Distributed only by permission.
 *)

IMPORT Wr, Fmt;
FROM Stdio IMPORT stdout;
<*FATAL ANY*>

TYPE
  Proc = PROCEDURE (i: INTEGER) RAISES {};

CONST
  ProcArray = ARRAY OF Proc{
    Proc1,
    Proc2,
    Proc3
  };

PROCEDURE Proc1(i: INTEGER) RAISES {} =
BEGIN
  Wr.PutText(stdout, "Proc1 (" & Fmt.Int (i) & ")\n");
END Proc1;

PROCEDURE Proc2(i: INTEGER) RAISES {} =
BEGIN
  Wr.PutText(stdout, "Proc2 (" & Fmt.Int (i) & ")\n");
END Proc2;

PROCEDURE Proc3(i: INTEGER) RAISES {} =
BEGIN
  Wr.PutText(stdout, "Proc3 (" & Fmt.Int (i) & ")\n");
END Proc3;

BEGIN
  FOR i := FIRST(ProcArray) TO LAST(ProcArray) DO
    ProcArray[i](i);
  END; (* for *)
END Main.


