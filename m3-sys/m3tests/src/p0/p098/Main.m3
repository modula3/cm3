(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********************************************************
Return-Path: <harrison@kodak.pa.dec.com>
Received: by jumbo.pa.dec.com; id AA23356; Wed, 20 Nov 91 13:29:06 -0800
Received: by kodak.pa.dec.com; id AA02310; Wed, 20 Nov 91 13:29:34 -0800
Received: by gossamer.pa.dec.com; id AA23307; Wed, 20 Nov 91 13:29:33 -0800
Message-Id: <9111202129.AA23307@gossamer.pa.dec.com>
To: kalsow
Subject: Is this legal Modula-3?
Date: Wed, 20 Nov 91 13:29:32 -0800
From: "Stephen Harrison" <harrison>
X-Mts: smtp


Bill,

How does the 2.0 compiler deal with the module below?  Is it legal?  I
wanted the signature of `Proc' to cover all the procedures initializing
/ProcArray/.  But it doesn't in 1.6.

One more thing ... can you remind me why /ProcArray/ can't be open? 
That is, why can I write

        CONST
          a1 = ARRAY OF INTEGER{1, 2, 3, 4};

but not

        VAR
          a2 := ARRAY OF INTEGER{5, 6, 7, 8};

?

Cheers,

/Stephen

---------------------------------------8<------------------------------------
***************************************************)

MODULE Main;

IMPORT Wr, Thread;
FROM Stdio IMPORT stdout;
<*FATAL Wr.Failure, Thread.Alerted, E1, E2*>

EXCEPTION
  E1;
  E2;

TYPE
  Proc = PROCEDURE (i: INTEGER) RAISES {E1, E2};

VAR
  ProcArray := ARRAY [1 .. 3] OF Proc{
    Proc1,
    Proc2,
    Proc3
  };

PROCEDURE Proc1(<*UNUSED*> i: INTEGER) RAISES {E1} =
BEGIN
  Wr.PutText(stdout, "Proc1\n");
END Proc1;

PROCEDURE Proc2(<*UNUSED*> i: INTEGER) RAISES {E2} =
BEGIN
  Wr.PutText(stdout, "Proc2\n");
END Proc2;

PROCEDURE Proc3(<*UNUSED*> i: INTEGER) RAISES {E1, E2} =
BEGIN
  Wr.PutText(stdout, "Proc3\n");
END Proc3;

BEGIN
  FOR i := FIRST(ProcArray) TO LAST(ProcArray) DO
    ProcArray[i](i);
  END; (* for *)
END Main.

