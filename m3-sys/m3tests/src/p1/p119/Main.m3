(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*******************************************
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA17938; Thu, 13 Feb 92 00:38:59 -0800
Received: by inet-gw-1.pa.dec.com; id AA05415; Thu, 13 Feb 92 00:38:47 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11527>; Thu, 13 Feb 1992 00:38:06 PST
Received: by arcturia.parc.xerox.com id <3177>; Thu, 13 Feb 1992 00:39:15 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: compiler bug
Message-Id: <92Feb13.003915pst.3177@arcturia.parc.xerox.com>
Date: 	Thu, 13 Feb 1992 00:39:09 PST

The following program prints "0 0 0" instead of "0 1 2".
**************************************************************)

MODULE Main;

IMPORT Wr, Stdio, Fmt;
IMPORT Thread;

PROCEDURE Put(txt: TEXT) =
<* FATAL Wr.Failure, Thread.Alerted *>
BEGIN
  Wr.PutText(Stdio.stdout, txt);
  Wr.Flush(Stdio.stdout);
END Put;

TYPE
  Enum = {one, two, three};

EXCEPTION Bad(Enum);

PROCEDURE Main (arg: Enum) =
  BEGIN
    TRY
      RAISE Bad(arg);
    EXCEPT
      Bad (n) =>
	Put(Fmt.Int(ORD(n)) & "\n");
    END;
  END Main;

BEGIN
   Main(Enum.one);
   Main(Enum.two);
   Main(Enum.three);
END Main.

