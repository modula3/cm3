(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(***********************************
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA01773; Mon, 17 Feb 92 18:24:06 -0800
Received: by inet-gw-1.pa.dec.com; id AA29970; Mon, 17 Feb 92 18:24:04 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11709>; Mon, 17 Feb 1992 18:24:01 PST
Received: by arcturia.parc.xerox.com id <3177>; Mon, 17 Feb 1992 18:25:56 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: compiler bug
Message-Id: <92Feb17.182556pst.3177@arcturia.parc.xerox.com>
Date: 	Mon, 17 Feb 1992 18:25:49 PST

The following program prints out

   2.299999
   4.799999

instead of 

   2.299999
   2.299999

*******************************************)

MODULE Main;

IMPORT Wr, Stdio, Fmt;
<*FATAL ANY*>

PROCEDURE Foo (VAR b: REAL) =
  BEGIN
    b := 2.3;
  END Foo;

PROCEDURE Main1 (b: REAL) =
  BEGIN
    Foo(b);
    Wr.PutText(Stdio.stdout, Fmt.F("%s\n", Fmt.Real(b)));
    Wr.Flush(Stdio.stdout);
  END Main1;

PROCEDURE Main (<*UNUSED*> b: REAL) =
  VAR c: REAL;
  BEGIN
    Foo(c);
    Wr.PutText(Stdio.stdout, Fmt.F("%s\n", Fmt.Real(c)));
    Wr.Flush(Stdio.stdout);
  END Main;

BEGIN
  Main(0.0);
  Main1(0.0);
END Main.


