(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**************************
Return-Path: <goldberg@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA00513; Tue, 21 Jan 92 12:25:12 -0800
Received: by inet-gw-1.pa.dec.com; id AA04593; Tue, 21 Jan 92 12:25:06 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11832>; Tue, 21 Jan 1992 12:07:12 PST
Received: by arcturia.parc.xerox.com id <3172>; Tue, 21 Jan 1992 12:05:47 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: 2.0beta bug
Message-Id: <92Jan21.120547pst.3172@arcturia.parc.xerox.com>
Date: 	Tue, 21 Jan 1992 12:05:37 PST

The following program won't compile, with the error

    "t1_m.c", line 9: syntax error at or near constant 1
    "t1_m.c", line 9: unknown size

Here's the program:

*****************************)

MODULE Main;
 
PROCEDURE Baz () =
    PROCEDURE Bar () = BEGIN END Bar;
  BEGIN
    Bar ();
  END Baz;

BEGIN
  Baz ();
  PROCEDURE Foo() =
    BEGIN
    END Foo;
  BEGIN
    Foo();
  END;
END Main.
 
 


