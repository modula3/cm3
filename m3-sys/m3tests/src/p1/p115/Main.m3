(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*****************************************************
Return-Path: <goldberg@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA23720; Sat, 21 Dec 91 23:43:31 -0800
Received: by mts-gw.pa.dec.com; id AA12867; Sat, 21 Dec 91 23:43:26 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11923>; Sat, 21 Dec 1991 23:41:54 PST
Received: by arcturia.parc.xerox.com id <3177>; Sat, 21 Dec 1991 23:42:18 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: compiler bug
Message-Id: <91Dec21.234218pst.3177@arcturia.parc.xerox.com>
Date: 	Sat, 21 Dec 1991 23:42:10 PST

This seems like a compiler bug.  The following program prints
	: :
instead of just one :.

*************************************************************)

MODULE Main;

IMPORT Wr, Stdio, Fmt;
<*FATAL ANY*>

PROCEDURE Foo(<*NOWARN*> first, second: ARRAY OF CHAR) =
  BEGIN
    Wr.PutText(Stdio.stdout, Fmt.F("%s %s\n",
				   Fmt.Char(first[0]), Fmt.Char(second[0])));
  END Foo;

BEGIN
  Foo(first := ARRAY [0..0] OF CHAR{' '},
      second := ARRAY [0..0] OF CHAR{':'});
END Main.


